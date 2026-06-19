SHELL := /bin/bash
ROOT := ..
TUTORIALS := $(ROOT)/tutorials
MKDOCS_DOCS := mkdocs/docs
EXAMPLES_DST := $(MKDOCS_DOCS)/examples
VENV := .venv

TUTORIAL_NAMES := QueueModel OptionPricing GeneTranscription
TUTORIAL_OUTPUTS := $(addprefix $(EXAMPLES_DST)/, $(addsuffix .md, $(TUTORIAL_NAMES)))

.PHONY: docs serve clean

docs: $(TUTORIAL_OUTPUTS)
	. $(VENV)/bin/activate && cd mkdocs && mkdocs build

serve: $(TUTORIAL_OUTPUTS)
	. $(VENV)/bin/activate && cd mkdocs && mkdocs serve

$(EXAMPLES_DST)/%.md: $(TUTORIALS)/%.lhs | $(EXAMPLES_DST)
	pandoc --from=markdown+lhs --to=commonmark $< -o $@

$(EXAMPLES_DST):
	mkdir -p $@

clean:
	rm -rf mkdocs/site $(EXAMPLES_DST)/*.md
