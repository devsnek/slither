" Vim filetype plugin file
" Language:     slither
" Maintainer:   Gus Caplan
" URL:          https://github.com/devsnek/slither

setlocal iskeyword+=$ suffixesadd+=.sl

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= ' | setlocal iskeyword< suffixesadd<'
else
  let b:undo_ftplugin = 'setlocal iskeyword< suffixesadd<'
endif
