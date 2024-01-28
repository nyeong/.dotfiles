return function ()
    vim.g.tagbar_type_asciidoc = {
        ctagsbin = '/opt/homebrew/bin/ctags',
        ctagstype = 'asciidoc',
        sort = 0,
    }
    vim.g.tagbar_type_asciidoctor = {
        ctagsbin = 'ctags',
        ctagstype = 'asciidoctor',
        kinds = {
            'h:table of contents',
            'a:anchors:1',
            't:titles:1',
            'n:includes:1',
            'i:images:1',
            'I:inline images:1'
        },
        sort = 0,
    }
end
