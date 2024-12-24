CREATE TABLE [dbo].[items](
	[First] [varchar](50) NOT NULL,
	[Second] [varchar](50) NOT NULL
) ON [PRIMARY]
GO

Select count(*)/ 3
from(SELECT i0.[First], i0.[Second], a1.third
     FROM items i0
          cross apply(select (case when i1.First=i0.First then i1.Second else i1.First end) as third
                      from items i1
                      where(i1.First=i0.First Or i1.Second=i0.First)and not(i1.First=i0.First and i1.Second=i0.Second)) as a1
          cross apply(select (case when i1.First=i0.Second then i1.Second else i1.First end) as fourth
                      from items i1
                      where((i1.First=i0.Second and i1.Second=a1.third)Or(i1.Second=i0.Second and i1.first=a1.third))) as a2
     where i0.[First] LIKE 't%' or i0.[Second] LIKE 't%' or a1.third LIKE 't%')b