#!/usr/bin/perl -w
#
# EJ to JE dictionary converter for gene.dic [1/25/1999]
#
# Copyright (C) 1998-1999 Satoru Takabayashi  All rights reserved.
#     This is free software with ABSOLUTELY NO WARRANTY.
#
# You can redistribute it and/or modify it under the terms of 
# the GNU General Public License version 2.
#
# gene.dic を元に和英辞書を作成します。
# gene.dic の書式に合わせてそれなりの処理を行います。
# かなり書式に揺れがあるので調整するのはなかなか難解でした。
# Perl 5.003 以降が必要です。 JPerl で動くかは分かりません。
# このファイルは日本語 EUC でなければなりません。
#
# 使い方: jgene.pl [--compat] < gene.dic > jgene.dic
#         --compat: 互換モード
#   
#   Pentium 133MHz, 64MB のマシンで約 2分かかります。
#
# 既知の問題点
#     * ・・から積み荷をおろす	unload
#     *  ーとの交換で	in barter with ~
#     *  〜(いやなこと)を経験させる	put someone through ~
#   のように先頭が記号で始まるものをそのままにしている。
#


require 5.004;
use strict;
use IO::File;

my $CHOON     = "(?:[\xa1][\xbc])";  # ー
my $CHOON2    = "(?:[\xa1][\xc1])";  # 〜
my $NAKAGURO  = "(?:[\xa1][\xa6])";  # ・
my $HIRAGANA  = "(?:[\xa4][\xa1-\xf3])";
my $KANJI     = "(?:[\xb0-\xfe][\xa1-\xfe]|\xa1\xb9)";
my $CHAR      = "(?:[\xa1-\xfe][\xa1-\xfe]|[^\xa1-\xfe])";

my $KAKASI = "kakasi";
my %je;

my $Compatible = 0;
$Compatible = 1, shift @ARGV if defined($ARGV[0]) && $ARGV[0] eq "--compat";

$| = 1;
print STDERR "reading input...\n";
while(<>) {
    /(.*)\t(.*)/;
    my $term = $1;
    my $definition = $2;

    # 括弧内などのカンマをエスケイプしておく
    $definition =~ s/(\/.*)   /escape_commas($1)/e;
    $definition =~ s/(\(.*?\))/escape_commas($1)/ge;
    $definition =~ s/(\".*?\")/escape_commas($1)/ge;
    $definition =~ s/(\<.*?\>)/escape_commas($1)/ge;

    # ★マークの注釈を前の意味に繋げる
    #   cable lock	ワイヤ錠,★南京錠はpadlock
    $definition =~ s/, *(★)/$1/g;

    my @meanings = split(/, */, $definition);
    for my $meaning (@meanings) {
	# いい加減な変換を2重に行う
	$meaning = transform($meaning);
	$meaning = transform($meaning);

	# = マークならばその元の単語にくっつける
	# (二つ目以降の項目にこれがくると良くないんだけど)
	#   business trend	=economic trend,景気動向
	if ($meaning =~ /^=/) {
	    $meaning =~ s/\x0/,/g;
	    $term .= " $meaning";
	    next;
	}

	# エスケイプしておいたカンマを復活
	$meaning =~ s/\x0/,/g;

	# 先頭が日本語ならば登録する
	$je{$meaning} .= $term . ", " if $meaning =~ /^[\xa1-\xfe]/;
    }
}

# ひらがなの読みを KAKASI に作ってもらう and 出力
# IPC::Open2 はいまいちなので一旦ファイルに書き出す
if ($Compatible) {
    for my $tmp (sort keys (%je)) {
	$je{$tmp} =~ s/, *$//;
	print $tmp, "\t", $je{$tmp}, "\n";
    }
} else {
    print STDERR "working KAKASI...\n";
    my $fname = "tmp.$$";
    {
	my $fh = new IO::File;
	$fh->open("|$KAKASI -JH -KH > $fname") or die $!;
	for my $tmp (sort keys(%je)) {
	    if ($tmp =~ /^$CHAR*$KANJI/o) {
		print $fh $tmp, "\n";
	    } else {
		print $fh "\n";
	    }
	}
    }

    my @results = ();
    {
	my $fh = new IO::File;
	$fh->open("$fname") or die $!;
	@results = <$fh>;
    }

    print STDERR "outputting results to stdout...\n";
    my $i = 0;
    for my $tmp (sort keys (%je)) {
	$je{$tmp} =~ s/, *$//;
	$je{$tmp} =~ s/&/&amp;/g;
	$je{$tmp} =~ s/</&lt;/g;
	$je{$tmp} =~ s/>/&gt;/g;

	$results[$i++] =~ /^((?:$HIRAGANA|$CHOON|$CHOON2|$NAKAGURO)+)/o;
	my $yomi = $1;
	if ($yomi && $tmp =~ /^$CHAR*$KANJI/o) {
	    print "<K>$tmp</K>", "<K>$yomi</K>", $je{$tmp}, "\n";
	} else {
	    print "<K>$tmp</K>", $je{$tmp}, "\n";
	}
    }
    unlink $fname;
} 

# おしまい

sub escape_commas ($) {
    my ($str) = @_;
    $str =~ s/,/\x0/g;
    $str;
}

sub transform($) {
    my ($meaning) = @_;

    # 先頭の空白を捨てる
    $meaning =~ s/^ +//;
    
    # 見出しの数字部分を削除
    # bottle  1.ビン,瓶,2.ビンに入れる,ビンに入れて密封する,抑える
    $meaning =~ s/^(\d+\. *)|(\(\d+\) *)//;

    # (the 〜) を削除
    # brink of bankruptcy (the 〜)破産の瀬戸際
    $meaning =~ s/\(the .*?\)//;
	
    # 先頭の (a 〜) を削除
    # bottle of ~ (a 〜)一ビンの
    $meaning =~ s/^\(a .*?\)//;
	
    # 先頭の ? を後ろへ持っていく
    # BIL  ? 基準衝撃絶縁強度
    $meaning =~ s/^\? (.*)/$1 ?/;
    
    # 先頭の 【...】 を後ろへ持っていく
    #   British Columbia  【地名】ブリティッシュコロンビア;略:BC
    # 先頭の 《...》 を後ろへ持っていく
    #   beanery  1.《米会話》安レストラン,大衆食堂,2.《米暗黒街》留置場
    # 先頭の <...> を後ろへ持っていく
    #   benign  1.<人・性格などが>親切な,優しい,
    #           2.<気候・風土が>健康に良い,温和な
    # 先頭の (...) を後ろへ持っていく
    #   captivate  (〜の心を)捕える,魅惑する,人をうっとりさせる,心を奪う
    # 先頭の [...] を後ろへ持っていく
    #   ciao	《伊》[感]<話>チャオ,こんにちは,さようなら

    $meaning =~ s/^((?:(?:【$CHAR*?】)|(?:《$CHAR*?》)|(?:<.*?>)|(?:\(.*?\))|(?:\[.*?\]) *)+)(.*)/$2 $1/;


    # 先頭の "..." から "" を取る
    #   European Free Trade Association  "エフタ",欧州自由貿易連合;略:EFTA
    $meaning =~ s/"(.*?)"/$1/;
    
    $meaning;
}
