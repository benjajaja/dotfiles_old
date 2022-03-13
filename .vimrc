if has('nvim')
  call plug#begin('~/.local/share/nvim/plugged')

  " essentials
  Plug 'tpope/vim-repeat'
  Plug 'svermeulen/vim-easyclip'
  Plug 'kien/ctrlp.vim'
  Plug 'tpope/vim-surround'
  Plug 'scrooloose/nerdcommenter'
  Plug 'bkad/CamelCaseMotion'

  " tabs
  Plug 'kyazdani42/nvim-web-devicons'
  Plug 'romgrk/barbar.nvim'

  " LSP
  Plug 'neovim/nvim-lspconfig'
  Plug 'hrsh7th/cmp-nvim-lsp'
  Plug 'hrsh7th/cmp-buffer'
  Plug 'hrsh7th/cmp-path'
  Plug 'hrsh7th/cmp-cmdline'
  Plug 'hrsh7th/nvim-cmp'
  Plug 'hrsh7th/cmp-vsnip'
  Plug 'hrsh7th/vim-vsnip'
  Plug 'RishabhRD/popfix'
  Plug 'RishabhRD/nvim-lsputils'

  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

  " typescript
  Plug 'nvim-lua/plenary.nvim'
  Plug 'jose-elias-alvarez/null-ls.nvim'
  Plug 'jose-elias-alvarez/nvim-lsp-ts-utils'

  " elm
  " Plug 'elmcast/elm-vim'
  " Plug 'andys8/vim-elm-syntax'

  " glsl
  Plug 'beyondmarc/glsl.vim'
  " rustfmt
  Plug 'rust-lang/rust.vim'
  " To enable more of the features of rust-analyzer, such as inlay hints and more!
  Plug 'simrat39/rust-tools.nvim'

  " themes
  Plug 'folke/tokyonight.nvim', { 'branch': 'main' }
  Plug 'Yagua/nebulous.nvim'

  " python
  Plug 'psf/black', { 'branch': 'stable' }

  Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }
  call plug#end()
endif

lua require("cmp_config")
lua require("lsp_config")

syntax enable
highlight ColorColumn guifg=#440000
if has('termguicolors')
  set termguicolors
endif

" let g:tokyonight_style = "night"
" let g:tokyonight_italic_functions = 1
colorscheme nebulous
hi Normal guibg=#000000 ctermbg=0
hi TabLineSel guifg=#ffffff ctermbg=green
hi TabLineFill guifg=#888888 guibg=#440000
hi MatchParen cterm=NONE ctermfg=green ctermbg=lightgreen

" set mouse+=a
" breaks clipboard


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
filetype plugin indent on
autocmd FileType go setlocal noexpandtab shiftwidth=4 softtabstop=4 tabstop=4 shiftwidth=4 omnifunc=lsp#complete

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
map <C-K> <Plug>(wintabs_previous)
map <C-J> <Plug>(wintabs_next)
" map <C-[> <Plug>(wintabs_previous)
" map <C-]> <Plug>(wintabs_next)
map <C-T>c <Plug>(wintabs_close)
map <C-T>u <Plug>(wintabs_undo)
map <C-T>o <Plug>(wintabs_only)
map <C-W> <Plug>(wintabs_close)

" barbar
let bufferline = get(g:, 'bufferline', {})
let bufferline.icons = v:true
let bufferline.closable = v:false
let bufferline.icon_close_tab = '·'
let bufferline.icon_close_tab_modified = '●'
let bufferline.icon_custom_colors = v:true
let bufferline.maximum_padding = 1
" let bufferline.auto_hide = v:true
nnoremap <silent>    <C-PageUp> :BufferPrevious<CR>
nnoremap <silent>    <C-PageDown> :BufferNext<CR>
nnoremap <silent>    <C-H> :BufferMovePrevious<CR>
nnoremap <silent>    <C-L> :BufferMoveNext<CR>
nnoremap <silent>    <C-W> :BufferClose<CR>

call bufferline#highlight#setup()


" prettier
" let g:prettier#autoformat = 0
" if filereadable(findfile('prettier.config.js', '.;'))
  " echo "Using prettier..."
  " autocmd BufWritePre *.js,*.jsx,*mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.md,*.vue,*.yaml,*.html PrettierAsync
  " autocmd BufWritePre *.js,*.jsx,*.ts,*.tsx PrettierAsync
" endif
au BufNewFile,BufRead *.tsx set filetype=typescript.tsx

" ts syntax
" let g:vim_jsx_pretty_colorful_config = 1

autocmd BufWritePre *.go lua vim.lsp.buf.formatting_sync(nil, 1000)

" search selected text with //
vnoremap // y/\V<C-r>=escape(@",'/\')<CR><CR>
" replace word under cursor
nnoremap <Leader>s :%s/\<<C-r><C-w>\>/

" Coc
" TextEdit might fail if hidden is not set.
" set hidden

" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Give more space for displaying messages.
" set cmdheight=2

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

" leader key
let mapleader=";"

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
" inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
"                               \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
" Highlight the symbol and its references when holding the cursor.
" autocmd CursorHold * silent call CocActionAsync('highlight')
" show type hint in command area
" nmap <silent> K :call CocAction("doHover")<CR>
" show full diagnostics
" nmap <silent> I :call CocAction("diagnosticInfo")<CR>

" Remap keys for gotos
" nmap <silent> gd <Plug>(coc-definition)
" nmap <silent> gy <Plug>(coc-type-definition)
" nmap <silent> gi <Plug>(coc-implementation)
" nmap <silent> gr <Plug>(coc-references)
" nmap <silent> gn <Plug>(coc-diagnostic-next)
" Remap for do codeAction of current line
" nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
" nmap <leader>qf <Plug>(coc-fix-current)

" Remap for rename current word
" nmap <leader>rn <Plug>(coc-rename)
" end Coc

" remap 'mark' to gm
nnoremap gm m
" M to delete to end of line and yank
nmap M <Plug>MoveMotionEndOfLinePlug

let g:camelcasemotion_key = '<leader>'

let g:EasyClipShareYanks = 1

let g:rustfmt_autosave = 1
" already are checking with coc, no more error windows are necessary
" let g:rustfmt_fail_silently = 1

" autocmd BufWinLeave * !~/cursor-reset %:p
" augroup RestoreCursorShapeOnExit
    " autocmd!
    " autocmd VimLeave * set guicursor=a:hor20-blinkwait400-blinkoff400-blinkon400
" augroup END

" colors
" highlight Normal ctermfg=044
" highlight Constant ctermfg=172
" highlight NonText ctermfg=093
" highlight ColorColumn guifg=#222120

let g:go_fmt_command = "golines"
let g:go_fmt_options = {
    \ 'golines': '-m 100',
    \ }
