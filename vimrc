"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                        SETTINGS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" various settings from around the web
"
" Adding a new setting should always be accompanied with a short comment 
" that states the setting, the purpose and possible a short comment
" The following syntax must be used:
" [YYYY-MM-DD] SETTING_NAME
" PURPOSE COMMENT


" [2016-01-30] counts / number format
" set numberformat to decimal. Don't tread numbers with leading zero as octal.
set nrformats=

" [2015-02-08] filetype treat *.md files as markdown-files 
" autocmd BufNewFile,BufReadPost *.md set filetype=markdown

" [2015-02-08] command-line completion
" Better command-line completion
set wildmenu

" [2015-02-08] hightlighting 
" Better highlighting for searchresults
set hlsearch

" [2016-02-06]
" Set incsearch on to hightlight matches
set incsearch

" [2015-02-08] case sensitive search
" search will be cas insensitive except when using capital letters
set ignorecase
set smartcase

" [2015-02-08] indention
" Enable indenting (same indention as line before)
set autoindent

" [2015-02-08] mouse usage
" Enable use of mouse for all modes
set mouse=a

" [2015-02-08] line numbers
" Display line numbers on the left
set number

" [2015-02-08] color scheme
" Enable monokai color scheme
" We can also use molokai (make sure it is present first)
syntax enable
colorscheme monokai
"colorscheme molokai

" [2015-10-13] vim-airline color scheme
" Set dark color scheme for vim-airline
:let g:airline_theme="dark"

" [2015-02-08] regular expressions
" Turn magic on for regular expressions
set magic

" [2015-02-08] alerts for errors
" turn off sounds for errors
" set noerrorbells

" [2015-11-06]
" lets try visual error bell
set visualbell

" [2015-02-08] encoding
" set utf8 as standard encoding
set encoding=utf8

" [2015-02-08] tabs
" use smarttab
set smarttab

" [2015-09-19]
set tabstop=4
set shiftwidth=4
set expandtab
set softtabstop=0

" [2015-02-08] wrap lines
" wrap lines
set wrap

" [2015-02-08] cursor position
" horizontal line to indicate cursor position
set cursorline

" [2015-02-08] handling of long line (moving)
" move vertically by visual line instead of 'real' lines
" tread virtual lines to wrap long lines onto screen as normal lines
" [2016-02-06] handling of virtual lines
" remap gj and gk to address real lines.
" [2016-03-04] deactivate this because it is a penance when working with latex
" nnoremap j gj
" nnoremap k gk
" nnoremap gj j
" nnoremap gk k

" [2016-02-12] Settings for latex suite
" see http://www.kubieziel.de/blog/archives/1333-Die-vim-LaTeXSuite.html
" for more infos

" [2016-03-13] Set leader key from [\] to [,] as it is more convenient on
" DE-layout
"let mapleader = "\"
let maplocalleader = ","


" settings for grep. Grep is used for autocomplete.
set grepprg=grep\ -nH\ $*
" make sure that vim treats .tex files, as latex files
let g:tex_flavor="latex"
" use biber as bibtex backend
let g:Tex_BibtexFlavor = 'biber'
" create a .pdf-document as standard target
let g:Tex_DefaultTargetFormat = 'pdf'
" to built a correct .pdf-document with citations, bibliography and index, we
" must compile the file multiple times
let g:Tex_MultipleCompileFormats = 'pdf,dvi'
" set g:Tex_CompileRule_pdf to allow us to sync
let g:Tex_CompileRule_pdf = 'pdflatex -synctex=1 -interaction=nonstopmode $*'
" get correct servername (filename of .tex-file and drop .tex)
" let theuniqueserv = expand ('%:r')
" now bring everything together:
" let g:Tex_ViewRuleComplete_pdf = 'zathura -x "vim --servername '.theuniqueserv.' --remote + \%{line} \%{input}" $*.pdf 2>/dev/null &'
" use zathura as standard viewer for .pdf-documents
let g:Tex_ViewRule_pdf = 'zathura'
let g:Tex_ViewRuleComplete_pdf = 'zathura $*.pdf >/dev/null &'
" enable caching for citations
" to manually rebuilt cache use following command:
" TClearCiteHist
" For further information see: http://vim-latex.sourceforge.net/documentation/latex-suite/customizing-latex-completion.html#RememberCiteSearch
let g:Tex_Remember_Cite_Search=1
" [2016-02-14] set custom command completion for bibliography
" for more informations see: http://vim-latex.sourceforge.net/documentation/latex-suite/ls-completion-custom.html
let g:Tex_completion_bibliographystyle = 'authoryear-icomp,abbr,alpha,plain,unsrt'

" [2016-02-12] performance settings for latex suite (vimtex)
" for more infos see: https://sourceforge.net/p/vim-latex/mailman/message/32672109/
autocmd FileType tex :NoMatchParen
autocmd FileType tex setlocal nocursorline

" [2016-02-12] Also, to maximize performance I added settings for highlighting
" for more infos see http://vim.wikia.com/wiki/Fix_syntax_highlighting
" to manually fix highlighting use command benath
" : syntax sync fromstart
autocmd BufEnter * :syn sync minlines=250
autocmd BufEnter * :syn sync maxlines=500

" settings below should be made in vimtex file?
" let g:Tex_Menus = 0 "no Tex-menus
" let g:Tex_Packages = 0 "do not scan for packages

" [2016-02-08] activate matchit plugin
" extend functionality of %
runtime macros/matchit.vim


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                       PLUGINS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"			MY SECTION START
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Various plugins from around the web
"
" Adding a new plugin must always be accompanied from adding a short comment
" in a FIXED syntax that informs about the date, name, purpose. It CAN be
" extended by a short comment.
" SYNTAX: [YYYY-MM-DD] PLUGINNAME PURPOSE COMMENT

" [2016-03-13] add speeddating-plugin for vim as it seems to be a dependency
" for vim-orgmode
Plugin 'tpope/vim-speeddating'

" [2016-03-12] add orgmode for vim
Plugin 'jceb/vim-orgmode'

" [2016-02-14] plugin Latex Suite
Plugin 'vim-latex/vim-latex'
" Plugin 'LaTeX-Suite-aka-Vim-LaTeX'

" [2015-10-13] add another monokai color scheme
" Same thing: color need to be copyied to ~/.vim/colors
Plugin 'tomasr/molokai'

" [2015-10-13] add vim-airline for vim
Plugin 'bling/vim-airline'

" [2015-10-11] add tmuxline for fancier tmuxline and vim statusline
Plugin 'edkolev/tmuxline.vim'

" [2015-04-19] add vim-ansible-yaml to suppot ansible yaml syntax
Plugin 'chase/vim-ansible-yaml'

" [2015-04-18] switch to markdown-plugin from @tim pope@
Plugin 'tpope/vim-markdown'

" [2015-04-18] nerdtree fileexplorer
Plugin 'scrooloose/nerdtree'

" [2015-02-08] vim-monokai monokai colorscheme for syntaxhighlighting
Plugin 'sickill/vim-monokai'

" [2015-02-08] tabular + vim-markdown to enable markdown support
Plugin 'godlygeek/tabular'
"Plugin 'plasticboy/vim-markdown'

" [2015-12-25] git wrapper.
Plugin 'tpope/vim-fugitive'

" [2016-02-08] use surround-plugin
" plug-in makes editing brackets and ticks comfortable
Plugin 'tpope/vim-surround'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line
