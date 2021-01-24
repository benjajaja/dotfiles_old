if has('nvim')
  call plug#begin('~/.local/share/nvim/plugged')

  " essentials
  Plug 'tpope/vim-repeat'
  Plug 'svermeulen/vim-easyclip'
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
  " Plug 'prettier/vim-prettier', { 'do': 'yarn install' }
  " Plug 'pangloss/vim-javascript'
  " Plug 'maxmellon/vim-jsx-pretty'
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
  " elm
  Plug 'elmcast/elm-vim'
  " esoteric
  " Plug 'tidalcycles/vim-tidal'

  " glsl
  Plug 'beyondmarc/glsl.vim'
  " rustfmt
  Plug 'rust-lang/rust.vim'

  " themes
  Plug 'phanviet/vim-monokai-pro'
  Plug 'protesilaos/tempus-themes-vim'

  call plug#end()
endif

syntax enable
filetype plugin indent on

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
" set termguicolors
syntax enable


" search selected text with //
vnoremap // y/\V<C-r>=escape(@",'/\')<CR><CR>
" replace word under cursor
nnoremap <Leader>s :%s/\<<C-r><C-w>\>/

" Coc
" TextEdit might fail if hidden is not set.
set hidden

" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Give more space for displaying messages.
set cmdheight=1

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')
" show type hint in command area
nmap <silent> K :call CocAction("doHover")<CR>
" show full diagnostics
nmap <silent> I :call CocAction("diagnosticInfo")<CR>

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

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)
" end Coc

" remap 'mark' to gm
nnoremap gm m
" M to delete to end of line and yank
nmap M <Plug>MoveMotionEndOfLinePlug

func GitGrep(...)
  let save = &grepprg
  set grepprg=git\ grep\ -n\ $*
  let s = 'grep'
  for i in a:000
    let s = s . ' ' . i
  endfor
  exe s
  let &grepprg = save
endfun
command -nargs=? G call GitGrep(<f-args>)

let g:EasyClipShareYanks = 1

let g:rustfmt_autosave = 1
" already are checking with coc, no more error windows are necessary
" let g:rustfmt_fail_silently = 1

" autocmd BufWinLeave * !~/cursor-reset %:p
augroup RestoreCursorShapeOnExit
    autocmd!
    autocmd VimLeave * set guicursor=a:hor20-blinkwait400-blinkoff400-blinkon400
augroup END

let g:rehash256 = 1
colorscheme molokai
