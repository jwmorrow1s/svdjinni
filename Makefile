ifndef VERBOSE
.SILENT:
endif
PROJ=svdjinni
SRC="./src/$(PROJ).lisp"

run: $(PROJ)
	./$(PROJ)

$(PROJ): build
	ros build $(PROJ).ros

build:
	@echo '#!/bin/sh' > $(PROJ).ros
	@echo '#|-*- mode:lisp -*-|#' >> $(PROJ).ros
	@echo '#|' >> $(PROJ).ros
	@echo 'exec ros -Q -- $$0 "$$@"' >> $(PROJ).ros
	@echo '|#' >> $(PROJ).ros
	@echo '\n' >> $(PROJ).ros
	@cat "$(SRC)" >> $(PROJ).ros

clean:
ifneq (,$(wildcard $(PROJ).ros))
	@rm $(PROJ).ros
endif
ifneq (,$(wildcard $(PROJ)))
	@rm $(PROJ)
endif
