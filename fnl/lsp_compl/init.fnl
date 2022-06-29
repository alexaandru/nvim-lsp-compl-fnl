;; available opts: leading_debounce<int>, subsequent_debounce<int>, on_delete<bool>, fuzzy<bool>.
(local default-opts {:leading_debounce 25})
(local compl-ctx (require :lsp_compl.compl-ctx))
(local run-ctx (require :lsp_compl.run-ctx))
(local SNIPPET 2)

(fn ns-to-ms [ns]
  (* ns 1e-06))

(fn get-documentation [item]
  (let [docs item.documentation]
    (or (match (type docs)
          :string docs
          :table (?. docs :value)) "")))

(fn match-cmp [a b]
  (< (or a.user_data.sortText a.user_data.label)
     (or b.user_data.sortText b.user_data.label)))

;; fnlfmt: skip
(fn completion-item [item fuzzy]
  (let [info (get-documentation item)
        kind (or (. vim.lsp.protocol.CompletionItemKind item.kind) "")
        word (if (= kind :Snippet) item.label
                 (= item.insertTextFormat SNIPPET) ;; see doc.txt#snippet formats
                 (if item.textEdit (or item.insertText item.textEdit.newText)
                     item.insertText (if (< (length item.label) (length item.insertText))
                                         item.label
                                         item.insertText)
                     item.label)
                 (or (?. item.textEdit :newText) item.insertText item.label))]
    {: info : kind : word
     :user_data item
     :menu (or item.detail "")
     :equal (if fuzzy 0 1)
     :abbr item.label
     :empty 1 :icase 1 :dup 1}))

(fn completion-items [result _ fuzzy]
  (let [items (vim.lsp.util.extract_completion_items result)
        items (or items {})
        matches (icollect [_ item (pairs items)]
                  (completion-item item fuzzy))]
    (table.sort matches match-cmp)
    matches))

;; fnlfmt: skip
(fn adjust-start-col [lnum line items encoding] ;; see doc.txt#vim.fn.complete
  (let [ctx {} break #(do (set ctx.min-start-char nil) (set ctx.stop true))]
    (each [_ item (pairs items) :until ctx.stop]
      (if (and item.textEdit (= item.textEdit.range.start.line (- lnum 1)))
          (let [range item.textEdit.range]
            (if (and ctx.min-start-char (not= ctx.min-start-char range.start.character)) (break))
            (if (> range.start.character range.end.character) (break))
            (set ctx.min-start-char range.start.character))))
    (if ctx.min-start-char
        (if (= encoding :utf-8) (+ ctx.min-start-char 1)
            (+ (vim.str_byteindex line ctx.min-start-char (= encoding :utf-16)) 1)))))

(fn exp-avg [window warmup]
  (let [z {:count 0 :sum 0 :val 0}]
    (fn [sample]
      (if (< z.count warmup)
          (do
            (set z.count (+ z.count 1))
            (set z.sum (+ z.sum sample))
            (set z.val (/ z.sum z.count)))
          (let [factor (/ 2 (+ window 1))]
            (set z.val (+ (* z.val (- 1 factor)) (* sample factor)))))
      z.val)))

;; FIXME: magic numbers 10 10
(local compute-new-average (exp-avg 10 10))

(fn complete [params start lnum line col cursor-pos]
  (fn handler [err result ctx]
    (let [end (vim.loop.hrtime)
          dur (- end start)
          dur (ns-to-ms dur)
          client-id ctx.client_id]
      (set run-ctx.rtt-ms (compute-new-average dur))
      (set compl-ctx.pending_requests {})
      (assert (not err) (.. "completion error: " (vim.inspect err)))
      (if (not result) (print "No completion result")
          (let [curr-cursor (vim.api.nvim_win_get_cursor 0)
                curr-lnum (. curr-cursor 1)
                line-changed (not= curr-lnum lnum)
                mode (. (vim.api.nvim_get_mode) :mode)]
            (set compl-ctx.isIncomplete result.isIncomplete)
            (if (not (or line-changed (not (or (= mode :i) (= mode :ic)))))
                (let [client (vim.lsp.get_client_by_id client-id)
                      items (vim.lsp.util.extract_completion_items result)
                      client-enc (?. client :offset_encoding)
                      encoding (or client-enc :utf-16)
                      adjusted-col (adjust-start-col lnum line items encoding)
                      startbyte (or adjusted-col col)
                      opts (?. run-ctx.clients client-id :opts)
                      prefix (line:sub startbyte cursor-pos)
                      matches (completion-items result prefix opts.fuzzy)]
                  (vim.fn.complete startbyte matches)))))))

  (let [req vim.lsp.buf_request
        (_ cancel) (req 0 :textDocument/completion params handler)]
    cancel))

(fn trigger_completion []
  (run-ctx.reset-timer)
  (compl-ctx.cancel)
  (let [(lnum cursor-pos) (unpack (vim.api.nvim_win_get_cursor 0))
        line (vim.api.nvim_get_current_line)
        line-to-cursor (line:sub 1 cursor-pos)
        col (+ (vim.fn.match line-to-cursor "\\k*$") 1)
        params (vim.lsp.util.make_position_params)
        start (vim.loop.hrtime)]
    (set compl-ctx.last_request start)
    (let [cancel (complete params start lnum line col cursor-pos)]
      (table.insert compl-ctx.pending_requests cancel))))

(fn next-debounce [subsequent-debounce]
  (let [debounce-ms (or subsequent-debounce run-ctx.rtt-ms)]
    (if compl-ctx.last_request
        (let [ns-since-request (- (vim.loop.hrtime) compl-ctx.last_request)
              ms-since-request (ns-to-ms ns-since-request)]
          (math.max (* (- ms-since-request debounce-ms) (- 1)) 0))
        debounce-ms)))

;; fnlfmt: skip
(fn insert-char-pre [client-id]
  (let [opts (. (. run-ctx.clients client-id) :opts)
        pumvisible (= (tonumber (vim.fn.pumvisible)) 1)]
    (if pumvisible
        (when (or compl-ctx.isIncomplete opts.fuzzy)
          (run-ctx.reset-timer)
          ;; Calling vim.fn.complete will trigger `CompleteDone` for the active completion window;
          ;; → suppress it to avoid resetting the completion_ctx
          (set compl-ctx.suppress_completeDone true)
          (let [debounce-ms (next-debounce opts.subsequent_debounce)]
            (if (= debounce-ms 0) (vim.schedule trigger_completion)
                (run-ctx.new-timer debounce-ms trigger_completion))))
        (if (not run-ctx.timer)
            (let [char (vim.api.nvim_get_vvar :char)
                  triggers (or (. run-ctx.triggers (vim.api.nvim_get_current_buf)) {})
                  debounce-ms opts.leading_debounce]
              (each [_ entry (pairs triggers)]
                (let [(chars f) (unpack entry)]
                  (if (vim.tbl_contains chars char)
                      ;; TODO: can we push the reset to new-timer() ?
                      (run-ctx.new-timer debounce-ms #(do (run-ctx.reset-timer) (vim.schedule f)))))))))))

(fn text-changed-p []
  (set compl-ctx.cursor (vim.api.nvim_win_get_cursor 0)))

(fn text-changed-i []
  (let [cursor compl-ctx.cursor]
    (if (and (not run-ctx.timer) cursor)
        (let [current-cursor (vim.api.nvim_win_get_cursor 0)]
          (if (and (= (. current-cursor 1) (. cursor 1))
                   (<= (. current-cursor 2) (. cursor 2)))
              ;; FIXME: magic number 150
              (run-ctx.new-timer 150 trigger_completion)
              (not= (. current-cursor 1) (. cursor 1))
              (set compl-ctx.cursor nil))))))

(fn insert-leave []
  (run-ctx.reset-timer)
  (set compl-ctx.cursor nil)
  (compl-ctx.reset))

(fn apply-text-edits [bufnr lnum text-edits offset-encoding]
  ;; Text edit in the same line would mess with the cursor position
  (let [edits (vim.tbl_filter #(not= $.range.start.line lnum)
                              (or text-edits {}))]
    (vim.lsp.util.apply_text_edits edits bufnr offset-encoding)))

(fn expand_snippet [snippet]
  (let [(ok luasnip) (pcall require :luasnip)
        f (or (and ok luasnip.lsp_expand) (. vim.fn "vsnip#anonymous"))]
    (f snippet)))

(fn apply-snippet [item suffix]
  ;; TODO: move cursor back to end of new text?
  (if item.textEdit (expand_snippet (.. item.textEdit.newText suffix))
      item.insertText (expand_snippet (.. item.insertText suffix))))

(fn completion-item-resolve [item bufnr lnum offset-encoding]
  (fn handler [err result]
    (set compl-ctx.pending_requests {})
    (let [additional-text-edits (?. result :additionalTextEdits)]
      (if err (vim.notify err.message vim.log.levels.WARN)
          additional-text-edits
          (apply-text-edits bufnr lnum additional-text-edits offset-encoding))))

  (let [req vim.lsp.buf_request
        (_ cancel) (req bufnr :completionItem/resolve item handler)]
    cancel))

(fn complete-done [client-id resolve-edits]
  (if compl-ctx.suppress_completeDone
      (set compl-ctx.suppress_completeDone false)
      (let [completed-item (vim.api.nvim_get_vvar :completed_item)
            noop (or (not completed-item) (not completed-item.user_data))]
        (if noop (compl-ctx.reset)
            (let [(lnum col) (unpack (vim.api.nvim_win_get_cursor 0))
                  lnum (- lnum 1)
                  item completed-item.user_data
                  bufnr (vim.api.nvim_get_current_buf)
                  expand-snippet (and (= item.insertTextFormat SNIPPET)
                                      compl-ctx.expand_snippet)]
              (if expand-snippet ;; remove the already inserted word
                  (let [start-char (- col (length completed-item.word))
                        lines (vim.api.nvim_buf_get_lines bufnr lnum (+ lnum 1)
                                                          true)
                        line (. lines 1)
                        set-text vim.api.nvim_buf_set_text]
                    (set compl-ctx.suffix (line:sub (+ col 1)))
                    (set-text bufnr lnum start-char lnum (length line) [""])))
              (let [suffix compl-ctx.suffix
                    client (vim.lsp.get_client_by_id client-id)
                    offset-encoding (or (and client client.offset_encoding)
                                        :utf-16)]
                (compl-ctx.reset)
                (if expand-snippet (apply-snippet item suffix))
                (if item.additionalTextEdits
                    (apply-text-edits bufnr lnum item.additionalTextEdits
                                      offset-encoding)
                    (and resolve-edits (= (type item) :table))
                    (let [cancel (completion-item-resolve item bufnr lnum
                                                          offset-encoding)]
                      (table.insert compl-ctx.pending_requests cancel)))))))))

(fn accept_pum []
  (let [pv (tonumber (vim.fn.pumvisible))
        vis (> pv 0)]
    (set compl-ctx.expand_snippet vis)
    vis))

(fn group-name [client-id bufnr]
  (string.format "LspCompl%d-%d" client-id bufnr))

(fn detach [client-id bufnr]
  (let [group (group-name client-id bufnr)]
    (vim.api.nvim_del_augroup_by_name group))
  (let [c (. run-ctx.clients client-id)]
    (set c.num_attached (- c.num_attached 1))
    (if (= c.num_attached 0)
        (tset run-ctx.clients client-id nil))))

(fn signature-handler [err result ctx config]
  (let [conf (or (and config (vim.deepcopy config)) {})
        sig-handler (. vim.lsp.handlers :textDocument/signatureHelp)]
    (set conf.focusable false)
    (sig-handler err result ctx conf)))

(fn signature-help []
  (run-ctx.reset-timer)
  (let [params (vim.lsp.util.make_position_params)]
    (vim.lsp.buf_request 0 :textDocument/signatureHelp params signature-handler)))

(fn attach [client bufnr opts]
  (set-forcibly! opts (vim.tbl_extend :keep (or opts {}) default-opts))
  (when client.server_capabilities.completionProvider
    (let [settings (or (. run-ctx.clients client.id) {:num_attached 0})]
      (tset run-ctx.clients client.id settings)
      (set settings.num_attached (+ settings.num_attached 1))
      (set settings.opts opts))
    (let [group (group-name client.id bufnr)
          au vim.api.nvim_create_autocmd
          au #(au $ {: group :buffer bufnr :callback $2})]
      (vim.api.nvim_create_augroup group {:clear true})
      (au :InsertCharPre #(insert-char-pre client.id))
      (when opts.on_delete
        (au :TextChangedP text-changed-p)
        (au :TextChangedI text-changed-i))
      (au :InsertLeave insert-leave)
      (local resolve-edits (?. client.server_capabilities.completionProvider
                               :resolveProvider))
      (au :CompleteDone #(complete-done client.id resolve-edits)))
    (var triggers (. run-ctx.triggers bufnr))
    (when (not triggers)
      (set triggers {})
      (tset run-ctx.triggers bufnr triggers)
      (vim.api.nvim_buf_attach bufnr false
                               {:on_detach #(tset run-ctx.triggers $2 nil)}))
    (let [signature-triggers (?. client.server_capabilities
                                 :signatureHelpProvider :triggerCharacters)]
      (if (and signature-triggers (> (length signature-triggers) 0))
          (table.insert triggers [signature-triggers signature-help])))
    (let [completion-provider client.server_capabilities.completionProvider
          completion-triggers (?. completion-provider :triggerCharacters)]
      (if (and completion-triggers (> (length completion-triggers) 0))
          (table.insert triggers [completion-triggers trigger_completion])))))

{: attach : detach : trigger_completion : expand_snippet : accept_pum}

