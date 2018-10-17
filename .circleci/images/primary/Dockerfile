FROM snoyberg/stackage:nightly

ENV PATH /usr/local/cuda-8.0/bin:/home/circleci/bin:/home/circleci/.local/bin:/home/circleci/ghc-head/bin:/usr/sbin:/usr/bin:/sbin:/bin

RUN apt-get install -qy s3cmd

# Remove stable GHC
RUN rm -rf /opt/ghc

# Create a normal user (this also sets HOME correctly otherwise CircleCI may
# be confused)
RUN adduser circleci --disabled-password
USER circleci
ENV HOME /home/circleci

# Create $HOME/bin
RUN mkdir -p $HOME/bin

# Get stack
RUN cd $HOME/bin && curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C $HOME/bin '*/stack'

# Update package indices
RUN stack update

# Build stackage curator
WORKDIR $HOME
RUN git clone https://github.com/fpco/stackage-curator.git
RUN cd $HOME/stackage-curator && git checkout eaeba273bdd38f121ad200b2a876227e666f86a2
RUN cd $HOME/stackage-curator && stack build --copy-bins

CMD ["bash"]
