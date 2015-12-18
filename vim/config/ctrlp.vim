let g:ctrlp_working_path_mode = 0
if executable('ag')
   let g:ctrlp_user_command = 'ag %s --files-with-matches --nocolor -g ""'
   let g:ctrlp_use_caching = 0
endif
