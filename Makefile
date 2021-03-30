all:
container: spin-ed-base.sif
executable: SpinED

spin-ed-base.sif: Singularity
	SINGULARITY_CACHEDIR=/scratch/twesterhout/singularity-cache \
	TMPDIR=/scratch/twesterhout/tmp \
	singularity build --force --fakeroot spin-ed-base.sif Singularity

SpinED: spin-ed-base.sif
	rm -rf container/
	SINGULARITY_CACHEDIR=/scratch/twesterhout/singularity-cache \
	TMPDIR=/scratch/twesterhout/tmp \
	singularity build --sandbox container spin-ed-base.sif
	cp container/project/spin-ed/SpinED-* .
	rm -rf container/
