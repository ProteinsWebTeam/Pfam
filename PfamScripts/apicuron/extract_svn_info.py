"""Extract information from Rfam SVN

This scripts extracts information from the Rfam SVN repository. The script was initially 
required to prepare for APICURON submissions. It is not required to run regularly, as we now
have a regaulr way to upload APICURON info via `bulk_upload.py`. However, if any information is lost or needs to be 
recreated, this script will be useful. 
"""

import datetime
import json
import re
import subprocess

import argparse

import conf


def get_author(revision, svn_url):
    """
    Run svnlook to get the author of the commit
    :param revision: svn repo revision number
    :param svn_url: filepath of the svn
    :return: author
    """

    author_cmd = "svnlook author -r {rev} {url}".format(rev=revision, url=svn_url)
    output = subprocess.check_output(author_cmd, shell=True, text=True)
    return output.strip()


def get_family(revision, svn_url):
    """
    Run svnlook to get the directory changed in the commit, then parse this to extract the family name
    :param revision: svn repo revision number
    :param svn_url: filepath of the svn
    :return: Rfam family name
    """

    family_cmd = "svnlook dirs-changed -r {rev} {url}".format(rev=revision, url=svn_url)
    output = subprocess.check_output(family_cmd, shell=True, text=True)
    #print(output)
    if match := re.search(r"(PF\d{5})", output):
        return "https://www.ebi.ac.uk/interpro/entry/pfam/" + match.group(1)
    elif match := re.search(r"(CL\d{4})", output):
        return "https://www.ebi.ac.uk/interpro/set/pfam/" + match.group(1)

    return ""


def get_term(revision, svn_url):
    """
    Run svnlook to get the message associated with the commit
    :param revision: svn repo revision number
    :param svn_url: filepath of the svn
    :return: the activity term, e.g. 'create_family'
    """

    message_cmd = "svnlook log -r {rev} {url}".format(rev=revision, url=svn_url)
    output = subprocess.check_output(message_cmd, shell=True, text=True)
    #print(output)
    activity_term = ""
    for ci_term, apicuron_term in conf.checkin_terms.items():
        #print(ci_term)
        if ci_term in output:
            #print(ci_term + ' in ' + output)
            activity_term = apicuron_term
    #print(activity_term)
    return activity_term


def get_timestamp(revision, svn_url):
    """
    Run svnlook to get the timestamp of the commit
    :param revision: svn repo revision number
    :param svn_url: filepath of the svn
    :return: timestamp, as string
    """

    date_cmd = "svnlook date -r {rev} {url}".format(rev=revision, url=svn_url)
    output = subprocess.check_output(date_cmd, shell=True, text=True)
    timestamp = output[:19]
    return timestamp


def write_report(args):
    reports_current = []
    reports_other = []
    url = args.svn
    prev_author = ''
    prev_author_orcid = ''
    prev_entity_uri = ''
    for rev in range(args.start, args.end):
        # print(rev)
        author = get_author(rev, url)
        apicuron_term = get_term(rev, url)
        if author == 'xfm_adm':
            author = prev_author
        else:
            prev_author = author
        author_orcid = conf.curator_orcids.get(author)
        if author_orcid == None:
            author_orcid = ''
        # print(author_orcid)
        entry = {
            "activity_term": apicuron_term,
            "timestamp": get_timestamp(rev, url),
            "curator_orcid": author_orcid,
            "entity_uri": get_family(rev, url),
        }
        #print(entry)
        if any(value == "" for value in entry.values()):
            # print("IGNORE NEXT:")
            # print(entry)
            next
        else:
            # print("SAVE NEXT:")
            # print(entry)
            if author in conf.curator_orcids:
                reports_current.append(entry)

    today_date = str(datetime.date.today())
    reports_file = "bulk_report_svn_" + today_date + ".json"
    with open(reports_file, "w") as bulk_report:
        reports = {"resource_id": "pfam", "reports": reports_current}
        json.dump(reports, bulk_report, indent=4, sort_keys=True)


def parse_args():
    """
    Parse the CLI arguments
    """
    
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--end", type=int, help="most recent revision number", action="store"
    )
    parser.add_argument(
        "--start",
        type=int,
        help="revision number to start from e.g. revision at last release",
        action="store",
    )
    parser.add_argument(
        "--svn", type=str, help="path to SVN repo to query", action="store"
    )
    return parser.parse_args()


def main():
    """
    Example usage:
    extract_svn_info.py --end 16099 --start 15938 --svn /path/to/svn
    """
    
    args = parse_args()
    #print(args)
    write_report(args)


if __name__ == "__main__":
    main()
