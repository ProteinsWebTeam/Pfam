#! /usr/bin/env python3

import os
import sys
import re
from shutil import copy2
import subprocess
import argparse

#Script to create RoseTTAfold tar archives for the ftp site
#This script will generate the following files:
#RoseTTAfold.full.tar.gz and RoseTTAfold.full.tar.gz.md5 (in cwd)
#RoseTTAfold.pdb.tar.gz and RoseTTAfold.pdb.tar.gz.md4 (in a cwd/RoseTTAfold/)
#Within RoseTTAfold dir, there will be 3 dir: a3m, pdb, npz. These can
#be deleted
#bsub the script the on the farm (1G memory, takes ~1 hour)
#If you need to copy models from the previous release, use the --previous_rosettafold_dir
#and --unchanged_list options
#Run with -h option to see usage



def main():

    #Parse user arguments
    parser = argparse.ArgumentParser(description='Create the RoseTTAfold.full.tar.gz and RoseTTAfold.pdb.tar.gz tar archives and md5 checksums for the ftp site')
    parser.add_argument('--rosettafold_dir', help='RoseTTAfold directory (the tar archive from Ivan, uncompressed and with files extracted)', required=True, type = valid_dir)
    parser.add_argument('--a3m_dir', help='Directory containing the A3M alignments that the structure models are based', required=True, type = valid_dir)
    parser.add_argument('--previous_rosettafold_dir', help='The RoseTTAfold directory from the previous release (the tar archive currently on the ftp site, uncompressed with files extracted), use this if you need to copy unchanged structural models from the last release', required=False, type = valid_dir)
    parser.add_argument('--unchanged_list', help='List of Pfam accessions whose seed alignment is unchanged since the last release, and therefore the model can be copied over from the previous release', required=False, type = argparse.FileType('r'))

    args = parser.parse_args()

    if(args.previous_rosettafold_dir or args.unchanged_list):
        if args.previous_rosettafold_dir is None or args.unchanged_list is None:
            parser.error("The --previous_rosettafold_dir and --unchanged_list option must be used together")


    #Make a dictionary of families for which we have a new structural model for
    #Could filter out low quality ones, but not doing this currently
    global_plddt_file = args.rosettafold_dir + "/lddt.txt"
    if not os.path.isfile(global_plddt_file):
        sys.exit(f"The file '{global_plddt_file}' does not exist")
    family_list = get_family_list(global_plddt_file)

    #Copy files (a3m, npz and pdb files) over for families that have a new RoseTTAfold structure model
    ftp_dir="RoseTTAfold"
    copy_files(family_list, args.rosettafold_dir, args.a3m_dir, ftp_dir)

    #Copy files from previous release (a3m, npz, pdb files) for RosetTTAfold structure models that are unchanged 
    #since the last release
    if(args.previous_rosettafold_dir):
        copy_old_files(args.unchanged_list, args.previous_rosettafold_dir, ftp_dir)

    #Make the tar archives, one containing everything, the other containing just the pdb files
    #Also create md5 checksums
    make_archives(ftp_dir)


def valid_dir(dir):
    if os.path.isdir(dir):
        return dir
    else:
        raise argparse.ArgumentTypeError(f"The directory '{dir}' does not exist")


def get_family_list(file):
    global_plddt = {}
    with open(file, 'r') as f:
        for line in f:
            match = re.search('^(\S+)\s+(\S+)',line)

            if match:
                pfamA_acc = match.group(1)
                plddt = match.group(2)

                global_plddt[pfamA_acc] = plddt

    return global_plddt


def copy_files(family_list, rosettafold_dir, a3m_dir, ftp_dir):

    #Make the directories
    if not os.path.exists(ftp_dir):
        os.mkdir(ftp_dir)

    os.chdir(ftp_dir)

    for dir in ('npz', 'pdb', 'a3m'):
        if not os.path.exists(dir):
            os.mkdir(dir)

    for pfamA_acc in family_list.keys():

        #copy pdb file
        if not os.path.exists('pdb/' + pfamA_acc + '.pdb'):
            pdb_file =  rosettafold_dir + '/pdb/' + pfamA_acc + '.pdb'
            copy2(pdb_file, 'pdb')

        #copy npz file
        if not os.path.exists('npz/' + pfamA_acc + '.npz'):
            npz_file =  rosettafold_dir + '/npz/' + pfamA_acc + '.npz'
            copy2(npz_file, 'npz')

        #copy a3m file
        if not os.path.exists('a3m/' + pfamA_acc + '.a3m'):
            a3m_file = a3m_dir + '/' + pfamA_acc + '.a3m'
            copy2(a3m_file, 'a3m')


    os.chdir('../')


def copy_old_files(unchanged_list, old_rosettafold_dir, ftp_dir):


    os.chdir(ftp_dir)

    #Go through families in the unchanged list and copy the npz, pdb and a3m files over
    with open(unchanged_list, 'r') as f:
            for line in f:
                match = re.search('^(PF\d{5})',line)

                if match:
                    pfamA_acc = match.group(1)

                    #copy pdb file
                    if not os.path.exists('pdb/' + pfamA_acc + '.pdb'):
                        pdb_file =  old_rosettafold_dir + '/pdb/' + pfamA_acc + '.pdb'
                        copy2(pdb_file, 'pdb')

                    #copy npz file
                    if not os.path.exists('npz/' + pfamA_acc + '.npz'):
                        npz_file =  old_rosettafold_dir + '/npz/' + pfamA_acc + '.npz'
                        copy2(npz_file, 'npz')

                    #copy a3m file
                    if not os.path.exists('a3m/' + pfamA_acc + '.a3m'):
                        a3m_file = old_rosettafold_dir + '/a3m/' + pfamA_acc + '.a3m'
                        copy2(a3m_file, 'a3m')

    os.chdir("../")


def make_archives(ftp_dir):

    #Make tar archive of everything
    if not os.path.exists('RoseTTAfold.full.tar.gz'):
        tar = subprocess.run(['tar', '-czvf', 'RoseTTAfold.full.tar.gz', ftp_dir], check=True, text=True, capture_output=True)
        if(tar.returncode != 0):
            print(tar.stderr)

    #Generate md5 checksum for full tar archive
    if not os.path.exists('RoseTTAfold.full.gz.md5'):
        with open('RoseTTAfold.full.tar.gz.md5', 'w') as outfile:
            md5 = subprocess.run(['md5sum', 'RoseTTAfold.full.tar.gz'], check=True, text=True, stdout=outfile)
            if(md5.returncode != 0):
                print(md5.stderr)


    os.chdir(ftp_dir)

    #Make tar archive of pdb files
    if not os.path.exists('RoseTTAfold.pdb.tar.gz'):
        tar = subprocess.run(['tar', '-czvf', 'RoseTTAfold.pdb.tar.gz', 'pdb'], check=True, text=True, capture_output=True)
        if(tar.returncode != 0):
            print(tar.stderr)

    #Generate md5 checksum for pdb tar archive
    if not os.path.exists('RoseTTAfold.pdb.gz.md5'):
        with open('RoseTTAfold.full.pdb.gz.md5', 'w') as outfile:
            md5 = subprocess.run(['md5sum', 'RoseTTAfold.pdb.tar.gz'], check=True, text=True, stdout=outfile)
            if(md5.returncode != 0):
                print(md5.stderr)

    os.chdir("../../")


if __name__ == '__main__':
    main()
