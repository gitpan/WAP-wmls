use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'          => 'WAP::wmls',
    'VERSION_FROM'  => 'node.pm',
    'ABSTRACT'      => 'WAP WMLScript compiler',
    'PREREQ_PM'     => {
                        'I18N::Charset'     => '1.15',
                        'Unicode::Map'      => 0,
                        'Unicode::String'   => 0,
    },
    'PM'            => {
                        'lexer.pm'      => '$(INST_LIBDIR)/wmls/lexer.pm',
                        'node.pm'       => '$(INST_LIBDIR)/wmls/node.pm',
                        'parser.pm'     => '$(INST_LIBDIR)/wmls/parser.pm',
                        'gen.pm'        => '$(INST_LIBDIR)/wmls/gen.pm',
                        'optim.pm'      => '$(INST_LIBDIR)/wmls/optim.pm',
                        'wmlslibs.cfg'  => '$(INST_LIBDIR)/wmls/wmlslibs.cfg',
    },
    'EXE_FILES'     => ['wmlsc', 'wmlsd'],
    'AUTHOR'        => "Francois PERRAD (perrad\@besancon.sema.slb.com)",
    'dist'          => {
                        'COMPRESS'      => 'gzip',
                        'SUFFIX'        => '.gz',
    },
);