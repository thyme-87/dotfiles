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

" [2015-02-08] filetype treat *.md files as markdown-files 
" autocmd BufNewFile,BufReadPost *.md set filetype=markdown

" [2015-02-08] command-line completion
" Better command-line completion
set wildmenu

" [2015-02-08] hightlighting 
" Better highlighting for searchresults
set hlsearch

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
syntax enable
colorscheme monokai

" [2015-02-08] regular expressions
" Turn magic on for regular expressions
set magic

" [2015-02-08] alerts for errors
" turn off sounds for errors
set noerrorbells

" [2015-02-08] encoding
" set utf8 as standard encoding
set encoding=utf8

" [2015-02-08] tabs
" use smarttab
set smarttab

" [2015-02-08] wrap lines
" wrap lines
set wrap

" [2015-02-08] cursor position
" horizontal line to indicate cursor position
set cursorline

" [2015-02-08] handling of long line (moving)
" move vertically by visual line instead of 'real' lines
" tread a long line that is visually wrapped as as many lines as it consists
" of
nnoremap j gj
nnoremap k gk

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
"			MY SECTION
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Various plugins from around the web
"
" Adding a new plugin must always be accompanied from adding a short comment
" in a FIXED syntax that informs about the date, name, purpose. It CAN be
" extended by a short comment.
" SYNTAX: [YYYY-MM-DD] PLUGINNAME PURPOSE COMMENT

" [2015-04-18] switch to markdown-plugin from @tim pope@
Plugin 'tpope/vim-markdown'

" [2015-04-18] nerdtree fileexplorer
Plugin 'scrooloose/nerdtree'

" [2015-02-08] vim-monokai monokai colorscheme for syntaxhighlighting
Plugin 'sickill/vim-monokai'

" [2015-02-08] tabular + vim-markdown to enable markdown support
Plugin 'godlygeek/tabular'
"Plugin 'plasticboy/vim-markdown'


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
