-- Check if module is available
function isModuleAvailable(name)
  if package.loaded[name] then
    return true
  else
    for _, searcher in ipairs(package.searchers or package.loaders) do
      local loader = searcher(name)
      if type(loader) == 'function' then
        package.preload[name] = loader
        return true
      end
    end
    return false
  end
end

modules = {
    -- Handle plugins with lazy.nvim
    "core.lazy",
    -- Set keymaps
    "core.keymaps",
    -- Set options
    "core.options",
    -- Configure with vimscript
    "core.vimscript",
    -- work related configuration
    "core.work",
}

for _, value in pairs(modules) do
    if isModuleAvailable(value) then
        require(value)
    end
end
