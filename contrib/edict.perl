#!/usr/local/bin/perl

# Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
# Keywords: dictionary

# EDICT形式の辞書をSDIC形式に変換する Perl スクリプト

# EDICT形式は、オーストラリア・Monash大学の Jim Breen 教授が収集されて
# いる和英辞書のフォーマットです。このフォーマットの辞書は、以下のURL
# から入手できます。
#
#     ftp://ftp.u-aizu.ac.jp/pub/SciEng/nihongo/ftp.cc.monash.edu.au/
#
# SDIC形式に変換する場合は、次のように利用して下さい。
#
#     perl edict.perl edict >edict.sdic
#
# SDIC形式の詳細については sdic.texi を参照して下さい。英和辞書に変換
# するためには、
#
#     perl edict.perl --reverse edict >edict.sdic
#
# と --reverse オプションを追加して実行して下さい。ただし、あまり良い
# 英和辞書は得られません。これを改良して下さる方を募集中です。


eval { binmode(STDOUT); };

if( $ARGV[0] eq '--reverse' ){
    shift;
    &reverse();
} else {
    &normal();
}
    
# 通常のSDIC形式の辞書を作る関数
sub normal {
    $_ = <>;				# 最初の1行は著作権表示である
    s!^[^/]+/!!;
    s!/$!!;
    s!\s*$!\n!;				# 改行コードを変換 [sdic:00428]
    print "# ",$_;
    while( <> ){
	s/\s+$//;			# 行末の空白文字を削除
	s!/\(P\)/$!!;			# 外来語のマークを削除する [sdic:00430]
	s/&/&amp;/g;			# メタキャラクタを置換する
	s/</&lt;/g;
	s/>/&gt;/g;
	s/^([\200-\377]+) +//;		# 見出し語を切り出す
	$head = $1;
	$key  = $head;
	while( s/^\[([\200-\377]+)\] +// ){
	    $key .= ">$1";
	}
	s!^/!!;
	s!/$!!;
	$KEY{ sprintf("%s>%s",$head,$_) } .= ">$key";
    }
    for $str ( keys %KEY ){
	( $head,$content ) = split( />/,$str );
	$KEY{$str} =~ s/^>//;
	for( split(/>/,$KEY{$str}) ){
	    $key{$_}++;
	}
	push( @LINE,sprintf( "<K>%s</K>%s%s\n",
			     $head,
			     join( "", map( sprintf("<K>%s</K>",$_),
					    grep( $_ ne $head, sort keys %key ) ) ),
			     $content ) );
	undef %key;
    }
    undef %KEY;
    print sort @LINE;
}

# 逆向きのSDIC形式の辞書を作る関数
sub reverse {
    $_ = <>;				# 最初の1行は著作権表示である
    s!^[^/]+/!!;
    s!/$!!;
    s!\s*$!\n!;				# 改行コードを変換 [sdic:00428]
    print "# ",$_;
    while( <> ){
	s/\s+$//;			# 行末の空白文字を削除
	s!/\(P\)/$!!;			# 外来語のマークを削除する [sdic:00430]
	s/&/&amp;/g;			# メタキャラクタを置換する
	s/</&lt;/g;
	s/>/&gt;/g;
	s/^([\200-\377]+) +//;		# 見出し語を切り出す
	$content = $1;			# 見出し語が説明文になる
	while( s/^\[([\200-\377]+)\] +// ){ ; }	# 振り仮名を捨てる
	s!^/!!;
	s!/$!!;
	for $head ( split( "/",$_ ) ){
	    $STR{$head} .= ">$content";
	}
    }
    for $head ( keys %STR ){
	for( split( />/,substr($STR{$head},1) ) ){
	    $content{$_}++;
	}
	for( keys %content ){
	    if( /^([\200-\377]{2})\1/ && $content{sprintf("%s々%s",$1,substr($_,4))} ){
		$content{$_}=0;
	    }
	}
	$str = $head;
	$str =~ tr/A-Z/a-z/;
	$str =~ s/\s+/ /;
	$key = $str;			# $key = 空白と英大文字/小文字の正規化を行なった文字列
	$str =~ s/^(\([^\)]+\) *)+//;
	$str =~ s/( *\([^\)]+\))+$//;
	$str =~ s/^~ //;
	$str =~ s/ ~$//;		# $str = (...) や ~ を取り除いた文字列
	$str = $key unless $str;
	if( $str eq $head ){
	    push( @LINE, sprintf( "%s\x00%s\x00<<K>%s</K>%s\n",
				  $str, $head, $head,
				  join("/",grep($content{$_}>0,keys %content)) ));
	} elsif( $key eq $head ){
	    push( @LINE, sprintf( "%s\x00%s\x00<<K>%s</K><K>%s</K>%s\n",
				  $str, $head, $head, $str,
				  join("/",grep($content{$_}>0,keys %content)) ));
	} elsif( $key eq $str ){
	    push( @LINE, sprintf( "%s\x00%s\x00<<H>%s</H><K>%s</K>%s\n",
				  $str, $head, $head, $str,
				  join("/",grep($content{$_}>0,keys %content)) ));
	} else {
	    push( @LINE, sprintf( "%s\x00%s\x00<<H>%s</H><K>%s</K><K>%s</K>%s\n",
				  $str, $head, $head, $str, $key,
				  join("/",grep($content{$_}>0,keys %content)) ));
	}
	undef %content;
    }
    undef %STR;
    for( sort @LINE ){
	@f = split( /</,$_,2 );
	print $f[1];
    }
}
