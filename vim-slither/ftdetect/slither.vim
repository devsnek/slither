fun! s:SelectSlither()
  if getline(1) =~# '^#!.*/bin/\%(env\s\+\)\?slither\>'
    set ft=slither
  endif
endfun

augroup  slither_syntax_detection
  autocmd!
  autocmd BufNewFile,BufRead *.{sl} setfiletype slither
  autocmd BufNewFile,BufRead * call s:SelectSlither()
augroup END
