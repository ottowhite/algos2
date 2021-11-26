BUILD_DIR = .tmp
OUT_DIR = out

.PHONY: check
check: clever-ai/Submission2.hs Lib.hs
	ghc -package mtl -iclever-ai clever-ai/AI.hs -outputdir $(BUILD_DIR) -fno-code

$(BUILD_DIR) :
	mkdir -p $(BUILD_DIR)

$(OUT_DIR) :
	mkdir -p $(OUT_DIR)

# server
.PHONY: server
server: $(BUILD_DIR) $(OUT_DIR)
	ghc -O2 Main.hs -DBUILD_DIR=\"$(BUILD_DIR)\" -DOUT_DIR=\"$(OUT_DIR)\" -outputdir $(BUILD_DIR)

test_results_info.json: AutoTest.hs .FORCE
	runghc -iclever-ai AutoTest.hs > test_results_info.json

.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)
	rm -rf $(OUT_DIR)
	rm -f Main


.PHONY: test
test: AutoTest.hs .FORCE
	runghc -iclever-ai AutoTest.hs -v > /dev/null

.PHONY: .FORCE
.FORCE:
