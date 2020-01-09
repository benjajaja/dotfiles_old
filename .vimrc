if has('nvim')
  call plug#begin('~/.local/share/nvim/plugged')

  " essentials
  Plug 'kien/ctrlp.vim'
  "Plug 'ap/vim-buftabline'
  Plug 'zefei/vim-wintabs'
  Plug 'zefei/vim-wintabs-powerline'
  Plug 'tpope/vim-surround'
  Plug 'scrooloose/nerdcommenter'
  " Plug 'tpope/vim-fugitive'
  " javascript
  Plug 'leafgarland/typescript-vim'
  " Plug 'HerringtonDarkholme/yats.vim'
  Plug 'prettier/vim-prettier', { 'do': 'yarn install' }
  " Plug 'pangloss/vim-javascript'
  " Plug 'maxmellon/vim-jsx-pretty'
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
  " elm
  Plug 'elmcast/elm-vim'
  " esoteric
  " Plug 'tidalcycles/vim-tidal'

  " scala
  Plug 'derekwyatt/vim-scala'

  call plug#end()
endif

let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git'
let g:ctrlp_custom_ignore = {
  \ 'dir':  'node_modules\|DS_Store\|git',
  \ 'file': '\v\.(class)$',
  \ }

set title
set hidden
set number
set cursorline

" tabs and spaces
set softtabstop=2
set tabstop=2
set shiftwidth=2
set expandtab
set ruler

" show whitespace anomalies
set list
set listchars=tab:>-,trail:~,extends:>,precedes:<

" nerdcommenter
let g:NERDSpaceDelims = 1
let g:NERDCommentEmptyLines = 1
let g:NERDTrimTrailingWhitespace = 1

" don't jump to start-of-line on buffer change
set nostartofline
set colorcolumn=80

" wintabs
let g:wintabs_ui_modified = " ≠"
"nnoremap <C-K> :bnext<CR>
"nnoremap <C-J> :bprev<CR>
"nnoremap <C-B> :bd<CR>
map <C-H> <Plug>(wintabs_previous)
map <C-L> <Plug>(wintabs_next)
" map <C-[> <Plug>(wintabs_previous)
" map <C-]> <Plug>(wintabs_next)
map <C-T>c <Plug>(wintabs_close)
map <C-T>u <Plug>(wintabs_undo)
map <C-T>o <Plug>(wintabs_only)
map <C-W> <Plug>(wintabs_close)

" prettier
let g:prettier#autoformat = 0
if filereadable(findfile('prettier.config.js', '.;'))
  echo "Using prettier..."
  " autocmd BufWritePre *.js,*.jsx,*mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.md,*.vue,*.yaml,*.html PrettierAsync
  autocmd BufWritePre *.js,*.jsx,*.ts,*.tsx PrettierAsync
endif
au BufNewFile,BufRead *.tsx set filetype=typescript.tsx

" ts syntax
" let g:vim_jsx_pretty_colorful_config = 1

" colors
" highlight Normal ctermfg=044
" highlight Constant ctermfg=172
" highlight NonText ctermfg=093
hi MatchParen    cterm=NONE ctermfg=green ctermbg=lightgreen


" search selected text with //
vnoremap // y/\V<C-r>=escape(@",'/\')<CR><CR>

" Coc
" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup
" Better display for messages
set cmdheight=2

" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c
" always show signcolumns
set signcolumn=yes
" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[c` and `]c` to navigate diagnostics
nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> gn <Plug>(coc-diagnostic-next)
" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf <Plug>(coc-fix-current)

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')
" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" show type hint in command area
nmap <silent> K :call CocAction("doHover")<CR>
" show full diagnostics
nmap <silent> I :call CocAction("diagnosticInfo")<CR>
