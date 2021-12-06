RUSTC=rustc -O
TIME=time
ALLRUST=$(wildcard day*/main.rs)

day%/rust: day%/main.rs
	$(RUSTC) $< -o $@

.PHONY: day%/rust-time
day%/rust-time: day%/rust
	echo "TIMING DAY $<"
	time $<
	echo ''

.PHONY: rust
rust: day$(DAY)/rust-time

.PHONY: clean
clean:
	$(RM) day*/rust

.PHONY: run
run: day$(D)/rust-time

.PHONY: all
all: $(subst main.rs,rust-time,$(ALLRUST))
