#!/usr/local/bin/perl

# Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
# Keywords: dictionary

# GENE95辞書を変換する Perl スクリプト

# GENE95辞書は、Kurumi さんが Nifty-Serve 英会話フォーラムで公開されて
# いる英和辞書です。「窓の杜」や SDIC のページから入手できます。
#
#     http://pine.kuee.kyoto-u.ac.jp/member/tsuchiya/elisp/gene.html
#
# COMPAT形式に変換する場合は、
#
#     nkf -S -e gene.txt | perl gene.perl --compat >gene.dic
#
# SDIC形式に変換する場合は、
#
#     nkf -S -e gene.txt | perl gene.perl >gene.sdic
#
# と指定して下さい。それぞれの形式の詳細については sdic.texi を参照。
#
# なお、COMPAT形式の辞書をSDIC形式に変換する場合は、
#
#    perl gene.perl --compat-to-sdic gene.dic >gene.sdic
#
# として下さい。
#
# SDIC形式の辞書をCOMPAT形式に変換する場合は、
#
#    perl gene.perl --sdic-to-compat gene.sdic >gene.dic
#
# として下さい。SDIC形式のほうが情報が豊かなので、この変換を行うと一般
# 的には情報が欠落しますので、注意して使って下さい。


eval { binmode(STDOUT); };

if(( $ARGV[0] eq '--compat' )){
    shift;
    &compat();
} elsif(( $ARGV[0] eq '--compat-to-sdic' )){
    shift;
    &compat_to_sdic();
} elsif(( $ARGV[0] eq '--sdic-to-compat' )){
    shift;
    &sdic_to_compat();
} else {
    &sdic();
}

# SDIC形式の辞書をつくる関数
sub sdic {
    $_ = <>;				# 2行読み飛ばす
    print "# ",$_;
    $_ = <>;
    print "# ",$_;
    for( $i=0; <>; $i++ ){
	s/\s+$//;			# 行末の空白文字を削除
	s/&/&amp;/g;			# メタキャラクタを置換する
	s/</&lt;/g;
	s/>/&gt;/g;
	if( $i%2==0 ){
	    $key = $_;
	}else{
	    $head = $key;
	    $key  =~ tr/A-Z/a-z/;
	    $key  =~ s/\s+/ /;
	    $key  =~ s/ \+\d+//;
	    if( $key eq $head ){
		push( @line, "$key\x00$head\x00<<K>$key</K>$_\n" );
	    } else {
		push( @line, "$key\x00$head\x00<<H>$head</H><K>$key</K>$_\n" );
	    }
	}
    }
    for( sort @line ){
	@f = split(/</,$_,2);
	print $f[1];
    }
}

# COMPAT形式の辞書を作る関数
sub compat {
    <>;					# 2行読み飛ばす
    <>;
    for( $i=0; <>; $i++ ){
	s/\s+$//;
	s/\t/        /g;
	if( $i%2==0 ){
	    $word = $_;
	}else{
	    $orig = $word;
	    $word =~ tr/A-Z/a-z/;
	    push( @line, "$word\x00$orig\x00\t$orig\t$_\n" );
	}
    }
    for( sort @line ){
	@f = split(/\t/,$_,3);
	print "$f[1]\t$f[2]";
    }
}

# COMPAT形式の辞書をSDIC形式に変換する関数
sub compat_to_sdic {
    while( <> ){
	s/\s+$//;			# 行末の空白文字を削除
	s/&/&amp;/g;			# メタキャラクタを置換する
	s/</&lt;/g;
	s/>/&gt;/g;
	@f = split( /\t/,$_,2 );
	$k = $f[0];
	$k =~ tr/A-Z/a-z/;
	$k =~ s/\s+/ /;
	$k =~ s/ \+\d+//;
	if( $k eq $f[0] ){
	    push( @line, "$k\x00$f[0]\x00<<K>$k</K>$f[1]\n" );
	} else {
	    push( @line, "$k\x00$f[0]\x00<<H>$f[0]</H><K>$k</K>$f[1]\n" );
	}
    }
    for( sort @line ){
	@f = split(/</,$_,2);
	print $f[1];
    }
}

# SDIC形式の辞書をCOMPAT形式に変換する関数
#     SDIC形式のほうが情報が多いため、COMPAT形式にすると、どうしても情
#     報の欠落が生じるため、注意して利用してください。
sub sdic_to_compat {
    while( <> ){
	next unless /^</;
	s/\s+$//;			# 行末の空白文字を削除
	s!^<([KH])>(.*)</\1>!!;		# 見出し語を取り出す
	$head = $2;
	$head =~ s/&lt;/</g;		# 見出し語のメタキャラクタを置換する
	$head =~ s/&gt;/>/g;
	$head =~ s/&amp;/&/g;
	while( s!^<K>(.*)</K>!! ){ ; }
	s/&lt;/</g;			# 説明文のメタキャラクタを置換する
	s/&gt;/>/g;
	s/&amp;/&/g;
	$key = $head;
	$key  =~ tr/A-Z/a-z/;
	push( @line, "$key\x00$head\x00\t$head\t$_\n" );
    }
    for( sort @line ){
	@f = split(/\t/,$_,3);
	print "$f[1]\t$f[2]";
    }
}
