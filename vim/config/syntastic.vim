let g:syntastic_check_on_open = 1
let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_javascript_checkers = ['jsxhint']
let g:syntastic_mode_map = {
      \ 'mode': 'active',
      \ 'active_filetypes': [],
      \ 'passive_filetypes': ['html', 'java']
      \ }
let g:syntastic_html_tidy_exec = 'tidy5'
