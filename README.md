# hive-ingestor-rfc

The IETF RFC series as a corpus for [hive-ingestor](../hive-ingestor).

The host owns the pipeline and its extension seams; this addon owns the
corpus. It registers one `ISource` factory in the host's source registry at
`initialize!` and retracts it at `shutdown!` — the host never names an RFC.

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
