use arxiculture;
# ok, which fields do I actually need
create table if not exists arxiv_metadata (
  id char(32),
  arxiv_id varchar(50) character set utf8, # 50 should be more than sufficient, right?
  authors varchar(1000) character set utf8,
  authors_hash char(32),
  submitted date,
  primary key (id),
  key (authors_hash, submitted)
);

