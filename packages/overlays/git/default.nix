{ writeText, ... }: { patches, ... }: {
  patches = patches ++ [ ./no-anon-stash.patch ];
}
