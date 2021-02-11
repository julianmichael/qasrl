from collections import namedtuple
import os.path
import urllib.request
# from __future__ import print_function
import sys
import time
import tarfile

# copied from:
# https://blog.shichao.io/2012/10/04/progress_speed_indicator_for_urlretrieve_in_python.html
def show_progress(count, block_size, total_size):
    global start_time
    if count == 0:
        start_time = time.time()
        return
    duration = time.time() - start_time
    progress_size = int(count * block_size)
    speed = int(progress_size / (1024 * duration))
    percent = int(count * block_size * 100 / total_size)
    sys.stdout.write("\r%d%%, %d MB, %d KB/s, %d seconds passed" %
                    (percent, progress_size / (1024 * 1024), speed, duration))
    sys.stdout.flush()

Dataset = namedtuple('Dataset', ['name', 'path', 'url', 'description'])

datasets = [
    Dataset(
        name = 'QA-SRL Bank 2.0',
        path = 'data/qasrl-v2',
        url = 'http://qasrl.org/data/qasrl-v2.tar',
        description = "Described in 'Large-Scale QA-SRL Parsing', "
        "FitzGerald et al., ACL 2018."
        "\nDeprecated in favor of v2.1 due to minor changes in slot definitions."
        "\nhttps://www.aclweb.org/anthology/P18-1191/"
    ),
    Dataset(
        name = 'QA-SRL Bank 2.1',
        path = 'data/qasrl-v2_1',
        url = 'http://qasrl.org/data/qasrl-v2_1.tar',
        description = "Updated from 2.0 to make minor fixes to slot definitions "
        "(see data/FORMAT.md)."
    ),
    Dataset(
        name = 'QANom',
        path = 'data/qanom',
        url = 'http://qasrl.org/data/qanom.tar',
        description = "Described in 'QANom: Question-Answer driven SRL for Nominalizations', "
        "\nKlein et al., COLING 2020."
        "\nhttps://www.aclweb.org/anthology/2020.coling-main.274/"
    ),
]
# TODO: upload commented datasets to site; QA-SRL-GS, QADiscourse

def get_dataset_option_prompt(num, dataset):
    if os.path.exists(dataset.path):
        color = "\u001b[32m"
        icon  = "[downloaded]"
    else:
        color = "\u001b[33m"
        icon  = "[not downloaded]"

    desc = ("\n" + dataset.description).replace("\n", "\n     ")

    return u"  {}) {}{} {}\u001b[0m ".format(num, color, dataset.name, icon) + desc + "\n"


def construct_prompt():
    prompt = "Which dataset would you like to download? ('all' to download all, 'q' to quit)"
    for i, dataset in enumerate(datasets):
        prompt += "\n" + get_dataset_option_prompt(i + 1, dataset)
    return prompt

def download_dataset(dataset):
    print("Downloading {}.".format(dataset.name))
    if(choice.url.endswith(".tar")):
       tarpath = choice.path + ".tar"
       urllib.request.urlretrieve(choice.url, tarpath, show_progress)
       result = tarfile.open(tarpath)
       result.extractall(os.path.dirname(choice.path))
       result.close()
       os.remove(tarpath)
    else:
       urllib.request.urlretrieve(choice.url, choice.path, show_progress)
    print("\nDownload complete: {}".format(dataset.path))

should_refresh_prompt = True
while True:
    if should_refresh_prompt:
        print(construct_prompt())
    print("Choose ({}-{}/all/q): ".format(1, len(datasets)), end='')
    should_refresh_prompt = False
    response = input()
    if "quit".startswith(response.lower()):
        break
    elif response.lower() == "all":
        for dataset in datasets:
            print(dataset.description)
            if os.path.exists(dataset.path):
                print("Already downloaded at {}.".format(dataset.path))
            else:
                download_dataset(dataset)
    else:
        try:
            choice = datasets[int(response) - 1]
        except ValueError or IndexError:
            print("Invalid option: {}".format(response))
            continue
        if os.path.exists(choice.path):
            print("Already downloaded at {}.".format(choice.path))
            print("Re-download? [y/N] ", end='')
            shouldDownloadStr = input()
            if shouldDownloadStr.startswith("y") or \
               shouldDownloadStr.startswith("Y"):
                download_dataset(choice)
        else:
            download_dataset(choice)
