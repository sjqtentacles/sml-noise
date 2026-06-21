# sml-noise build
#
#   make            build the test binary with MLton (default)
#   make test       build + run tests under MLton
#   make test-poly  run tests under Poly/ML (use-and-run; no link step)
#   make all-tests  run the suite under both compilers
#   make clean      remove build artifacts
#
# Layout B (dependent): own sources live in src/; sml-glm and sml-prng are
# vendored under lib/ and loaded first, in dependency order.

MLTON      ?= mlton
POLY       ?= poly
BIN        := bin
GLMDIR     := lib/github.com/sjqtentacles/sml-glm
PRNGDIR    := lib/github.com/sjqtentacles/sml-prng
TEST_MLB   := test/test.mlb
SRCS       := $(wildcard $(GLMDIR)/* $(PRNGDIR)/* src/* test/*.sml) $(TEST_MLB)

.PHONY: all test poly test-poly all-tests clean

all: $(BIN)/test-mlton

$(BIN)/test-mlton: $(SRCS) | $(BIN)
	$(MLTON) -output $@ $(TEST_MLB)

test: $(BIN)/test-mlton
	$(BIN)/test-mlton

# Poly/ML has no native .mlb support; the suite runs at top level and exits on
# its own. Load vendored deps first (glm, then prng), then the noise sources,
# then the test driver.
poly test-poly:
	printf 'use "$(GLMDIR)/glm.sig";\nuse "$(GLMDIR)/glm.sml";\nuse "$(PRNGDIR)/prng.sig";\nuse "$(PRNGDIR)/prng.sml";\nuse "src/noise.sig";\nuse "src/noise.sml";\nuse "test/harness.sml";\nuse "test/support.sml";\nuse "test/test_perlin.sml";\nuse "test/test_value_worley.sml";\nuse "test/test_fbm.sml";\nuse "test/test_continuity.sml";\nuse "test/entry.sml";\nuse "test/main.sml";\n' | $(POLY) -q --error-exit

all-tests: test test-poly

$(BIN):
	mkdir -p $(BIN)

clean:
	rm -f $(BIN)/test-mlton
