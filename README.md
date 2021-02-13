# qasrl

Repository for QA-SRL tools and data, including interaction, crowdsourcing, and the QA-SRL Bank 2.0.

## Contents

This repository contains several modules and data utilities.

 * `qasrl`: General tools for validating, manipulating, and autocompleting QA-SRL.
 * `qasrl-crowd`: UI and server code for for crowdsourcing QA-SRL data on Mechanical Turk.
 * `qasrl-crowd-example`: Standalone Mill project demonstrating how to use the crowdsourcing pipeline for your own data.
 * `qasrl-bank`: Client library for the QA-SRL Bank 2.0.
 * `qasrl-bank-service`: HTTP service and server implementations for using the QA-SRL Bank 2.0.
 * `apps/`: Several applications using QA-SRL, including a webapp for browsing the QA-SRL Bank 2.0.
 * `data/`: QA-SRL data documentation and datasets (downloadable interactively with `scripts/download_data.py`).

## Usage

The four modules `qasrl`, `qasrl-crowd`, `qasrl-bank`, and `qasrl-bank-service` are published on Maven Central. To use them, add e.g.,
```
  libraryDependencies += "org.julianmichael" %%% "qasrl" % "0.2.0"
```
to your sbt project settings, or 
```
  ivy"org.julianmichael::qasrl::0.2.0",
```
to your Mill `ivyDeps` (and similarly for the other modules). To inflect verbs you will need our Wiktionary scrape (either via `scripts/download_data.py`) or [here](https://www.dropbox.com/s/60hbl3py7g3tx12/wiktionary.tar.gz?dl=1).

To compile this project and run the apps or example project in this repo, you need
[Mill](http://www.lihaoyi.com/mill/index.html).

### Running the example project

Run `python scripts/download_data.py` and download the Wiktionary dataset.
Then run `qasrl-crowd-example/scripts/run_crowd_example.sh`.
Note that the example project uses an old version of the `qasrl` libraries as a dependency.
Unfortunately a recent dependency upgrade seems to have broken the crowdsourcing interfaces
and I haven't figured out the fix yet.

### Autocomplete

To get an idea of how the autocomplete functionality works, try running `scripts/run_autocomplete_example.sh`.
This will throw you into a REPL where you may type prefixes to questions (or full questions)
and get the autocomplete feedback displayed to you when you press Enter.

If you wish to use the autocomplete or other functionality in your own code,
take a look at `scripts/autocomplete_example.scala`, which was running in the previous step, for example usage.

### Crowdsourcing

To start up the crowdsourcing pipeline and see a preview of the task UI, run `scripts/run_crowd_example.sh` and go to
`localhost:8888/task/generation/preview` in your browser.
To understand how to set everything up so it works on MTurk,
adapt the instructions [here](https://github.com/uwnlp/qamr/tree/master/code)
to the local code. To trace the main entry points:

 * [`scripts/crowd_example.scala`](https://github.com/julianmichael/qasrl-crowdsourcing/blob/master/qasrl-crowd-example/scripts/crowd_example.scala)
   is what you run on the SBT console to get started.
 * That creates an [`AnnotationSetup`](https://github.com/julianmichael/qasrl/blob/master/qasrl-crowd-example/example/src-jvm/example/AnnotationSetup.scala) object defined in `qasrl-crowd-example`,
   which assembles the various data and resources needed for the crowdsourcing pipeline.
 * That creates a [`QASRLAnnotationPipeline`](https://github.com/julianmichael/qasrl/blob/master/qasrl-crowd/src-jvm/qasrl/crowd/QASRLAnnotationPipeline.scala) object,
   which creates the web services and interfaces with MTurk to upload and download data and assess workers.
 * Finally, telling the `QASRLAnnotationPipeline` object to `start()` will start the crowdsourcing task.
 
Again, see the [instructions in the QAMR project](https://github.com/uwnlp/qamr/tree/master/code) for a more detailed account (though it is very slightly out of date).
