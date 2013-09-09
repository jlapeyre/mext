#!/usr/bin/env perl
use strict;
use warnings;
#use File::Copy;
use Getopt::Long;
use File::Spec::Functions;
#use Pod::Usage;

=head1 NAME 

I<rtest_renumber.pl> - renumber the tests in an rtest_xxx.mac file.

=head1 SYNOPSIS

  make_mext.pl  newpackname

=head1 DESCRIPTION

Creat template files for a mext package.
Create a directory named newpackname and populate it with files
for defining, building, and loading the mext package. The resulting
files are templates and need to be edited

=cut

my $Script = 'make_mext';
my $Package = shift(@ARGV);

sub make_package {
    if ( not -e $Package ) {
        mkdir $Package or die "$Script: Can't create directory '$Package': $!";
    }
    write_file(\&make_ibuild_mac, "ibuild.mac");
    write_file(\&make_package_system, "$Package.system");
    write_file(\&make_package_mxt, "$Package.mxt");
    write_file(\&make_package_mac, "$Package.mac");
}

sub write_file {
    my ($filesub, $filename) = @_;
    my $str = $filesub->($Package);
    my $filepath = catfile($Package,$filename);
    open my $ofh, '>', $filepath or die "$Script: Can't open open $filepath for writing: $!";
    print $ofh $str or die "$Script: Can't write contents to $filepath: $!";
    close($ofh);
}

sub make_ibuild_mac {
    my ($pack) = @_;

my $fstr = <<"EOFS";
/* Template. This file must be edited */
/* Load this file in maxima to build mext package $pack. */
load(mext);
require(""); /* mext packages that $pack depends on */
load_in_subdir(["$pack","system"]);
mext_dist_clean();
mext_dist_build();
mext_dist_user_install();
mext_dist_clean();
EOFS

}

sub make_package_system {
    my ($pack) = @_;

    my $fstr1 = <<"EOFS";
;;-*- Mode: Lisp -*-
;; Template. This file must be edited
;; System definition file for building and installing mext package $pack

(in-package :mext-maxima)

(create-distribution "$pack"
  :loader "load-$pack"
  :install-only "$pack-rtests")

;; Here we list the source files to be installed.
;; Choose "lisp" or "mac" as the default source file extension.
;; The source extension may be overridden.
;; All files are compiled unless overidden with :load-only
(mk:defsystem $pack
  :source-extension "lisp"
;  :source-extension "mac"  ; uncomment these two lines for maxima source
;  :language :mext-maxima
  :components
   ((:file "file1" :source-extension "mac" :load-only t)
    (:file "file2")))

;; This file will load $pack at run time.
(mk:defsystem load-$pack
  :source-extension "mac"
  :components
   ((:file "$pack")))

;; Here we list the rtest files for $pack.
;; Put one or more files here that begin with "rtest_"
(mk:defsystem $pack-rtests
              :source-extension "mac"
              :source-pathname "rtests"
              :components
              ((:file "rtest_$pack"))) ; for example
EOFS
    
}

sub make_package_mxt {
    my ($pack) = @_;

   return <<"EOFS";
;;-*- Mode: Lisp -*-
;; Template. This file must be edited
(in-package :mext)

(distribution-description
   :name   "$pack"
   :author "Author Name"
   :maintainer "Maintainer Name"
   :version "0.0.1"
   :license "GPL2+"
   :description "Functions for $pack."
   :long-description "Functions for $pack.")
EOFS
}


sub make_package_mac {
    my ($pack) = @_;

   my $fstr2 = <<"EOFS";
/* Template. This file must be edited */
/* This file loads mext package $pack at run time. */
/* Replace the following a mext package, or list of them, on which $pack depends at runtime. */
/* Or else, remove it. */
require(""); 
/* List files to be loaded: source, lisp, or binary, without extensions. */
mext_provide("$pack",
  [ "file1",
    "file2"]);
EOFS
}



make_package();
