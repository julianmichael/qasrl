# qasrl-crowdsourcing

Repository for QA-SRL tools, particularly for interaction and crowdsourcing.

## Contents

There are currently three projects in this repository.

 * `qasrl`: General tools for validating, interpreting, and autocompleting QA-SRL.
 * `qasrl-crowd`: UI and server code for for crowdsourcing QA-SRL data on Mechanical Turk.
 * `qasrl-crowd-example`: Example SBT project showing how to use the crowdsourcing pipeline for your own data.

## Usage

Since I have not published this code on Maven or anything, it is easiest to use it from source,
for example, as a submodule in your project.
Clone it to your machine and run `scripts/setup.sh`.
This will download the `nlpdata` and `spacro` dependencies and publish them locally to your Ivy cache so this will compile.
It will also prompt you to download a Wiktionary dataset, which will be placed at `datasets/wiktionary`.
This will be necessary to run the QA-SRL validation and autocomplete, crowdsourcing, etc.

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

 * [`scripts/crowd_example.scala`](https://github.com/julianmichael/qasrl-crowdsourcing/blob/master/scripts/crowd_example.scala)
   is what you run on the SBT console to get started.
 * That creates an [`AnnotationSetup`](https://github.com/julianmichael/qasrl-crowdsourcing/blob/master/qasrl-crowd-example/jvm/src/main/scala/example/AnnotationSetup.scala) object defined in `qasrl-crowd-example`,
   which assembles the various data and resources needed for the crowdsourcing pipeline.
 * That creates a [`QASRLAnnotationPipeline`](https://github.com/julianmichael/qasrl-crowdsourcing/blob/master/qasrl-crowd/jvm/src/main/scala/qasrl/crowd/QASRLAnnotationPipeline.scala) object,
   which creates the web services and interfaces with MTurk to upload and download data and assess workers.
 * Finally, telling the `QASRLAnnotationPipeline` object to `start()` will start the crowdsourcing task.
 
Again, see the [instructions in the QAMR project](https://github.com/uwnlp/qamr/tree/master/code) for a more detailed account.

### Misc

Finally, to use this project in yours, either add it as a source dependency or run
`sbt publishLocal` and add it as a managed dependency in your desired project.
