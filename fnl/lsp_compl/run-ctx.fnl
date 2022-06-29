(local ctx {:rtt-ms 50 :clients {} :triggers {}})

(fn ctx.new-timer [ms f]
  (set ctx.timer (vim.loop.new_timer))
  (ctx.timer:start ms 0 (vim.schedule_wrap f)))

(fn ctx.reset-timer []
  (when ctx.timer
    (ctx.timer:stop)
    (ctx.timer:close)
    (set ctx.timer nil)))

ctx

