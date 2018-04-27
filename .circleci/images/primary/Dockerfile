FROM snoyberg/stackage:nightly

ENV PATH /usr/local/cuda-8.0/bin:/home/circleci/bin:/home/circleci/ghc-head/bin:/usr/sbin:/usr/bin:/sbin:/bin

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

# Get Stackage curator
RUN cd $HOME/bin && curl https://s3.amazonaws.com/stackage-travis/stackage-curator/stackage-curator.bz2 --output stackage-curator.bz2
RUN cd $HOME/bin && bunzip2 stackage-curator.bz2
RUN cd $HOME/bin && chmod +x stackage-curator

# Get stack
RUN cd $HOME/bin && curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C $HOME/bin '*/stack'

# Update package indices
RUN stack update

CMD ["bash"]
