# hive-ingestor-rfc

The IETF RFC series as a corpus for a hive ingestion pipeline.

The pipeline owns chunking, embedding and the graph; this addon owns the
corpus. It is written against the public seam, not the pipeline:
[`hive-spi.ingest`](https://github.com/hive-agi/hive-spi) for the `ISource`
port, the Document and the owner-scoped registry, and
[`hive-ingest-kit`](https://github.com/hive-agi/hive-ingest-kit) for
`body->document`, so a mirrored RFC and one fetched over HTTP are parsed by
the same rule chain. It registers one `ISource` factory at `initialize!` and
retracts it at `shutdown!`; the host never names an RFC.

## Mirror

Bulk access to the series is rsync, not HTTP:

```
rsync -avz --delete rsync.rfc-editor.org::rfcs-text-only <dir>
```

~10k documents / ~550 MB.

## Ingesting

```
ingest source source=rfc dir=<mirror> from=7000 to=7999
```

A run is a *window* (`from` / `to` / `offset` / `limit`). Re-running a window
upserts rather than duplicating, because chunk and document ids are derived
from the canonical rfc-editor URL — which is also why a mirrored document and
one fetched over HTTP are the same document to the store and the KG.

## Developing

```
clj -Sdeps "$(cat local.deps.edn)" -M:test
```

with an untracked `local.deps.edn` pointing `hive-spi`, `hive-html` and
`hive-ingest-kit` at sibling checkouts when you are working on the seam.

## License

MIT.
