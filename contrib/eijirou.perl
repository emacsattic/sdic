#!/usr/local/bin/perl

# Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
# Keywords: dictionary

# 『英辞郎』を変換する Perl スクリプト

# 英辞郎は、非常に巨大なテキストベースの辞書です。しかし、Internet 上
# では配布されておらず、Nifty-Serve からダウンロードするか、CD-ROM ま
# たは書籍を購入する必要があります。
#
# COMPAT形式に変換する場合は、
#
#     nkf -S -e [file]... | perl eijirou.perl --compat >eijirou.dic
#
# SDIC形式に変換する場合は、
#
#     nkf -S -e [file]... | perl eijirou.perl >eijirou.sdic
#
# と指定して下さい。それぞれの形式の詳細については sdic.texi を参照。

$CHAR = "(?:[\xa1-\xfe][\xa1-\xfe]|[^\xa1-\xfe])";
$HIRAGANA = "(?:[\xa4][\xa1-\xf3])";

for( @ARGV ){
    if( $_ eq '--unsort' ){
	$UNSORT = 1;
    } elsif( $_ eq '--compat' ){
	$COMPAT = 1;
    } elsif( $_ eq '--waei' ){
	$WAEI = 1;
    } else {
	push( @tmp, $_ );
    }
}
@ARGV = @tmp;

eval { binmode(STDOUT); };

if( $COMPAT ){
    &compat();
} else {
    &sdic();
}

# COMPAT形式の辞書を生成する関数
sub compat {
    while( <> ){
	s/\s+$//;			# 行末の改行コードを削除
	s/\t/        /g;		# タブを空白8文字に置換
	s/^[\200-\377]{2}//;		# 行頭の全角文字を削除
	s/(\{[^\}]+\}) : / : $1 /;	# {…} を説明文に移動する
	( $key,$content ) = split( / +: /,$_,2 );
	$head = $key;
	$key =~ tr/A-Z/a-z/;
	if( $UNSORT ){
	    print "$head\t$content\n";
	} else {
	    push( @line, "$key\x00$head\x00\t$head\t$content\n" );
	}
    }
    unless( $UNSORT ){
	for( sort @line ){
	    @f = split(/\t/,$_,3);
	    print "$f[1]\t$f[2]";
	}
    }
}

# SDIC 形式の辞書を生成する関数
sub sdic {
    while( <> ){
	s/\s+$//;			# 行末の改行コードを削除
	s/&/&amp;/g;			# メタキャラクタを置換する
	s/</&lt;/g;
	s/>/&gt;/g;
	s/^[\200-\377]{2}//;		# 行頭の全角文字を削除
	( $head,$content ) = split( / +: /,$_,2 );
	$key = $head;
	$key =~ s/ +\{[^\}]+\}//;	# {…} を検索キーから削除する
	$key =~ s/ +\(\d+\)//;		# (…) を検索キーから削除する
	$key =~ tr/A-Z/a-z/;
	$key =~ s/\s+/ /;
	if( $WAEI ){
	    # 和英辞朗に特有の調整を行う
	    while( $content =~ s/^($CHAR*?)●/$1 \/ /o ){ ; }
	    $key =~ s/^($CHAR*?)；(?:（$CHAR*?）|〜)$HIRAGANA?$/$1/o;
	    $key =~ s/^($CHAR*?)◆$CHAR*?$/$1/o;
	}
	if( $UNSORT ){
	    if( $key eq $head ){
		print "<K>$key</K>$content\n";
	    } else {
		print "<H>$head</H><K>$key</K>$content\n";
	    }
	} else {
	    if( $key eq $head ){
		push( @line,"$key\x00$head\x00<<K>$key</K>$content\n" );
	    } else {
		push( @line,"$key\x00$head\x00<<H>$head</H><K>$key</K>$content\n" );
	    }
	}
    }
    unless( $UNSORT ){
	for( sort @line ){
	    @f = split(/</,$_,2);
	    print $f[1];
	}
    }
}
