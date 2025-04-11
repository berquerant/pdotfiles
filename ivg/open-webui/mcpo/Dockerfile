FROM docker:cli

WORKDIR /workspace
COPY setup.sh config.json entrypoint.sh /workspace/
RUN /workspace/setup.sh
CMD ["/workspace/entrypoint.sh"]
