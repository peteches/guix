
M = {}
function M.dispatch_and_reset(dispatcher)
  return function()
    hl.dispatch(dispatcher)
    hl.dispatch(hl.dsp.submap("reset"))	
  end
end

function M.dispatch_many_and_reset(dispatchers)
  return function()
    for _, dispatcher in ipairs(dispatchers) do
      hl.dispatch(dispatcher)
    end
    hl.dispatch(hl.dsp.submap("reset"))
  end
end


return M
