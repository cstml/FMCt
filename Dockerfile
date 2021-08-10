# Checkout the official Nix Image.
FROM nixos/nix

# Copy the current folders contents.
COPY ./ ./app

# Set the work directory to the folder.
WORKDIR  ./app

# Run nix-build
RUN nix-build

# Set a port
ENV PORT=8080

# On Run run this command
CMD ["./result/bin/FMCt-web",""]
