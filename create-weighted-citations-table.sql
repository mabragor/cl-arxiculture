use arxiculture;
# ok, which fields do I actually need
create table if not exists weighted_cites (
  referring_paper binary(16),
  cited_paper binary(16),
  weight float,
  key (referring_paper),
  key (cited_paper)
);
