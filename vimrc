"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                        SETTINGS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" switch from date based comments about changes to other format:
" Any change will must be commented.
" The comment must inform about the purpose of the change. What shall the
" change achieve?
" Changes should be implemented step by step and ideally a commit should
" follow every commit to allow finer granularity when it comes to merging

set t_Co=256        "set vim to 256 colors
set nrformats=alpha "set number format to decimal; numbers with leading zero won't be treated as octal
set wildmenu        "better command-line completion
set hlsearch        "better hightlighting for searchresults
set incsearch       "highlight matches while typing searchterm
set ignorecase      "no case sensitivity when searching
set smartcase       "enable case sensitivity for searchterms that start with capital letters?
set autoindent      "enable auto indenting TODO currently indention for latex files seems broken; someone proposed to use `set breakindent` instead of autoindent
set formatoptions+=2    "Use indent from 2nd line of a paragraph "it could be, that formatoption+=2 is what I have been looking for all along for .text files
set mouse=a         "enable mouse for all modes
set number          "display always line numbers
syntax enable       "enable syntax highlighting
set magic           "turn on regular expressions
set visualbell      "use visual notifications for errors instead of annoying sound
set encoding=utf-8      "set encoding to UTF-8
set fileencoding=utf-8  "set fileencoding to UTF-8
set smarttab            "use smart tabs
set tabstop=4           "a tab equals four spaces
set shiftwidth=4
set expandtab           "expand tabs with spaces
set softtabstop=0
"wrap lines             "unset because it hurts readability with VIM-ORGMODE for further informations on wrapping see http://vim.wikia.com/wiki/Word_wrap_without_line_breaks for further
set wrap                    "break text with virtual new lines instead of hard ones
set linebreak               "wrap long lines at characters defined in `breakat` (next line)
set breakat=" !@*-;:,./?"   "linebreaks shall only happen after complete words! For more infos see `:help breakat`
set breakindent             "wrapped line will continue visually indented.For further informations see `:help breakindent`
set nolist
set textwidth=0
set wrapmargin=0
set formatoptions-=t
set cursorline      "horizontal line to indicate cursor position
set diffopt=filler  "add vertical spaces to keep splits aligned
set diffopt+=iwhite "ignore white space
set nostartofline   "don't reset the cursor to start of line
set noshowmode      "only works when it is at the bottom of the .vimrc - i have now idea why :(

set timeoutlen=800  "set mapping delays
set ttimeoutlen=0   "set keycode delays

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                           KEY MAPPINGS                            "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set leader and localleader
"let mapleader = "\"
"TODO currently localleader and leader are mapped to the same key. That is not
"clever
let maplocalleader = ","    "set localleacer to ","
let mapleader = ","         "set leader to ","

" key mappings in normal mode for navigation between tabs
nnoremap th :tabfirst<CR>
nnoremap tl :tablast<CR>
nnoremap <c-h> :tabprevious<CR>
nnoremap <c-l> :tabnext<CR>

" remap keys for moving up and down linewise instead of virtual line wise
" nnoremap j gj
" nnoremap k gk
" nnoremap gj j
" nnoremap gk k

" Keymappings for actions
nnoremap <F12> :NERDTreeToggle<CR>          "<F12> to toggle Nerdtree
nnoremap <F11> :!detex % \| wc -w<CR>       "<F11> for simple wordcount
nnoremap <silent> <Leader>t :TagbarToggle<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                 SELF DEFINED COMMANDS                             "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
com! FormatJSON %!python -m json.tool   "reformat current buffer as JSON file
com! DotDisplay :call DotDisplay()                                      
com! DotToPdf   :call DotToPdf()                                        
com! MarkdownRender :call MarkdownRender()                              "render markdown using pandoc
com! MarkdownDisplay :call MarkdownDisplay()                            "open the according .pdf-file with zathura
com! UpdateDictonaries :call UpdateDictionaries()                       "call self defined function to update all dictonaries based on .add files in dotfiles/vim/spell

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                 SELF DEFINED FUNCTIONS                            "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! DotDisplay()
    :silent :execute '!coproc dot -Tx11 %'
    redraw!
endfunction

function! DotToPdf()                                                    "use arguments for different programs (algorithms)
    :silent :execute '!coproc dot -Tpdf -O %'
    redraw!
endfunction

function! MarkdownRender()                                              "currently the process is not executed asynchronously
                                                                        "TODO: add arguments for table of content, formatting etc.
    :silent :execute '!coproc pandoc --toc -S -s -f markdown -o %:p.pdf %'
    redraw!
endfunction

function! MarkdownDisplay()
    :silent :execute '!coproc zathura %:p.pdf'
    redraw!
endfunction

function! UpdateDictionaries()                                          "Update all spellfiles based on .add-files in dotfiles/vim/spell
    :silent
    for d in glob('~/.vim/spell/*.add', 1, 1)
        if filereadable(d) && (!filereadable(d . '.spl') || getftime(d) > getftime(d . '.spl'))
            exec 'mkspell! ' . fnameescape(d)
        endif
    endfor
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                 SETTINGS FOR SPECIFIC FILETYPES                   "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"LATEX
let tlist_tex_settings = 'latex;l:labels;s:sections;t:subsections;u:subsubsections'

"--------------------------------------------------------------------

" MARKDOWN
let g:tagbar_type_markdown = {
    \ 'ctagstype' : 'markdown',
    \ 'kinds' : [
        \ 'h:Heading_L1',
        \ 'i:Heading_L2',
        \ 'k:Heading_L3'
    \ ]
\ }
au BufNewFile,BufRead,BufEnter      README      setlocal spell  spelllang=en_us "set spell check for README files
" au BufNewFile,BufRead,BufEnter      *.md        setlocal spell  spelllang=de_de "set spellcheck with language de_de for markdown files currently deactivated as I assume that it would break settings for markdown beneath
autocmd BufNewFile,BufRead,BufEnter *.md setlocal filetype=markdown textwidth=80 
autocmd BufNewFile,BufRead,BufEnter *.md nnoremap <Leader>t :Voomtoggle<CR>
"set Voomtoggle only for md files; TODO: set also for .tex file: set also for .tex files
autocmd BufNewFile,BufReadPost *.md call voom#Init("markdown",1)    "use voom#Init function to generate Tree
autocmd BufWritePost *.md call voom#BodyUpdateTree()     "update the tree after the file has been saved
autocmd BufWritePost *.tex call voom#BodyUpdateTree()    "update the tree after the file has been saved

"PHP
let g:tagbar_phpctags_bin='/usr/bin/phpctags'
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                  SETTINGS FOR SPECIFIC PLUGINS                    "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" VIM VOom
let g:voom_ft_modes = {'markdown': 'markdown', 'tex': 'latex'}

" VIM ALE
let g:airline_section_error = '%{ALEGetStatusLine()}' "ALE output in vim-airline
let g:airline#extensions#ale#enabled=1
let g:ale_linters = {
    \'php': ['phpcs'],
    \    }
let g:ale_php_phpcs_standard = 'PSR2'
let g:ale_statusline_format = ['✗%d', '⚠%d', '☼ok']
let g:ale_echo_cursor=1
let g:ale_echo_msg_format = '[%linter%]: %s [%severity%]'
let g:ale_set_loclist = 1
let g:ale_sign_error = '✗»'
let g:ale_sign_warning = '⌕☞'
let g:ale_echo_msg_error_str = 'Error'
let g:ale_echo_msg_warning_str  = 'Warning'
nmap <silent> <leader>] <Plug>(ale_previous_wrap)
nmap <silent> <leader>[ <Plug>(ale_next_wrap)

" GITGUTTER
set updatetime=250  "set interval in which gitgutter is updated

" THEMING
colorscheme monokai "use monokai colorscheme; alternative: molokai
"let g:molokai_original = 1     "specific setting for molokai

" VIM-AIRLINE
let g:airline_powerline_fonts = 1       "use fonts patched for powerline
let g:airline_theme="dark"              "use dark theme
let g:airline#extensions#tagbar#enabled = 0 "disable tagbar integration (performance issue)
set laststatus=2

" VIM LATEX SUITE
" for more infos see: http://www.kubieziel.de/blog/archives/1333-Die-vim-LaTeXSuite.html
set grepprg=grep\ -nH\ $*                       "settings for grep; used for autocomplete
let g:tex_flavor="latex"                        "make sure vim treats *.tex files as latex files
let g:Tex_BibtexFlavor = 'biber'                "use biber as bibtex backend
let g:Tex_Packages = 0                          "do not scan for packages
let g:Tex_Menus = 0 "no Tex-menus "don'T show tex menues - this setting
let g:Tex_DefaultTargetFormat = 'pdf'           "PDF document as standard target for compiling
let g:Tex_MultipleCompileFormats = 'pdf,dvi'    "compile multiple times for pdf and dvi files (to build index and citations correctly)
let g:Tex_CompileRule_pdf = 'pdflatex -synctex=1 -interaction=nonstopmode $*' "allow us to sync
" let theuniqueserv = expand ('%:r')            "get correct servername
" let g:Tex_ViewRuleComplete_pdf = 'zathura -x "vim --servername '.theuniqueserv.' --remote + \%{line} \%{input}" $*.pdf 2>/dev/null &'
let g:Tex_ViewRule_pdf = 'zathura'              "use zathura as standard viewer for PDF documents
let g:Tex_ViewRuleComplete_pdf = 'zathura $*.pdf >/dev/null &'
let g:Tex_Remember_Cite_Search=1                "enable caching for citations
" to manually rebuilt cache for citation use `TCLEARCiteHIST` command
" for further informations see:http://vim-latex.sourceforge.net/documentation/latex-suite/customizing-latex-completion.html#RememberCiteSearch
let g:Tex_completion_bibliographystyle = 'authoryear-icomp,abbr,alpha,plain,unsrt' "set custom command completion for bibliography
" for more informations see: http://vim-latex.sourceforge.net/documentation/latex-suite/ls-completion-custom.html

" [2016-02-12] performance settings for latex suite (vimtex)
" performance  settings for VIM LATEX SUITE
" for more infos see: https://sourceforge.net/p/vim-latex/mailman/message/32672109/
autocmd FileType tex :NoMatchParen          "don't match parenthesis
autocmd FileType tex setlocal nocursorline  "don't set a cursorline for .tex files
autocmd FileType tex let b:tagbar_ignore = 1    "disable tagbar for tex files
autocmd FileType tex let g:neocomplete#enable_at_startup = 0 "disable neocomplete for tex files
autocmd BufEnter * :syn sync minlines=250   "for t400 don't parse whole file for syntax hightlighting
autocmd BufEnter * :syn sync maxlines=500   "fix syntax hightlighting with `: syntax sync fromstart`
" for more infos see http://vim.wikia.com/wiki/Fix_syntax_highlighting
" MATCHIT PLUGIN
runtime macros/matchit.vim  "extend functionality of "%"


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

" [2017-06-21] PHP autocompletion
Plugin 'shawncplus/phpcomplete.vim'

" [2016-09-20] Support for R:
Plugin 'jalvesaq/Nvim-R'

" [2017-06-14] Linter for various languages
Plugin 'w0rp/ale'

" [2016-05-24] plugin to display tags in a window (ordered by scope)
" deactivatet to check performance
Plugin 'majutsushi/tagbar'

" [2016-05-24] dark color scheme for vim
" See http://vimawesome.com/plugin/hybrid-vim for instructions to set the
" right color palette in ~.Xressources
Plugin 'w0ng/vim-hybrid'

" [2016-05-24] plugin for seamless navigation between tmux, panes and splits
" deactivatet to check performance
" Plugin 'christoomey/vim-tmux-navigator'

" [2016-05-24] better hightlighting for JSON
Plugin 'elzr/vim-json'

" [2016-05-24] plugin to work with multiple cursors
" See http://vimawesome.com/plugin/vim-multiple-cursors for instructions
" deactivatet to check performance
" Plugin 'terryma/vim-multiple-cursors'

" [2016-05-24] alternativ to ctrlp that supports plugins
Plugin 'shougo/unite.vim'

" [2016-05-24] fuzzy search for files, buffers, tags and MRUs (most recent
" used)
" deactivatet to check performance
" Plugin 'ctrlpvim/ctrlp.vim'

" [2016-05-24] plugin for completion in vim
" Needs to be configured and tested
" https://github.com/Shougo/neocomplete.vim
" deactivatet to check performance
" Plugin 'Shougo/neocomplete'

" [2016-05-24] plugin to visualize diffs in the gutter
" deactivatet to check performance
Plugin 'airblade/vim-gitgutter'

" [2016-04-24] markdown folding
Plugin 'nelstrom/vim-markdown-folding'

" [2016-04-24] add VOoM (vim two-pane outliner)
Plugin 'VOom'

" [2016-04-24] add vim-outliner
Plugin 'vimoutliner/vimoutliner'

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
Plugin 'vim-airline/vim-airline'

" [2016-04-12] add vim-airline themes
Plugin 'vim-airline/vim-airline-themes'

" [2015-10-11] add tmuxline for fancier tmuxline and vim statusline
Plugin 'edkolev/tmuxline.vim'

" [2015-04-19] add vim-ansible-yaml to suppot ansible yaml syntax
Plugin 'chase/vim-ansible-yaml'

" [2015-04-18] nerdtree fileexplorer
Plugin 'scrooloose/nerdtree'

" [2015-02-08] vim-monokai monokai colorscheme for syntaxhighlighting
Plugin 'sickill/vim-monokai'

" [2015-02-08] tabular + vim-markdown to enable markdown support
Plugin 'godlygeek/tabular'

" [2015-04-18] switch to markdown-plugin from @tim pope@
"Plugin 'tpope/vim-markdown'

" [2016-05-24 another attempt with better syntaxhighlighting for markdown
Plugin 'plasticboy/vim-markdown'

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
set noshowmode
