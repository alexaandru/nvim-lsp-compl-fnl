(local ctx {})

(fn ctx.reset []
  ;; Cursor is not reset here, it needs to survive a `CompleteDone` event
  (set ctx.expand_snippet false)
  (set ctx.isIncomplete false)
  (set ctx.suppress_completeDone false)
  (set ctx.last_request nil)
  (set ctx.suffix nil)
  (ctx.cancel))

(fn ctx.cancel []
  (if ctx.pending_requests
      (each [_ cancel (pairs ctx.pending_requests)]
        (cancel))
      (set ctx.pending_requests {})))

(ctx.reset)

ctx

