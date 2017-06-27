/* Leetcode 262: Trips and Users */

use [test]
go

create table Trips
(
[Id] int,
[Client_Id] int,
[Driver_Id] int,
[City_Id] int,
[Status] varchar(50),
[Request_at] datetime
)

create table Users
(
[Users_Id] int,
[Banned] varchar(10),
[Role] varchar(20)
)

insert into Trips
values
(1, 1,10,1,'completed','20131001'),
(2, 2,11,1,'cancelled_by_driver','20131001'),
(3, 3,12,6,'completed','20131001'),
(4, 4,13,6,'cancelled_by_client','20131001'),
(5, 1,10,1,'completed','20131002'),
(6, 2,11,6,'completed','20131002'),
(7, 3,12,6,'completed','20131002'),
(8, 2,12,12,'completed','20131003'),
(9, 3,10,12,'completed','20131003'),
(10, 4,13,12,'cancelled_by_driver','20131003')

--select * from Trips

insert into Users
values
(1,'No','client'),
(2,'Yes','client'),
(3,'No','client'),
(4,'No','client'),
(10,'No','driver'),
(11,'No','driver'),
(12,'No','driver'),
(13,'No','driver')

--select * from Users

/* Write a SQL query to find the cancellation rate of requests made by unbanned clients between Oct 1, 2013 and Oct 3, 2013 */
with joined_cte
as
(
select t1.Client_Id, t1.[Status], t1.Request_at, t2.Banned from 
Trips t1 inner join 
Users t2
on t1.Client_Id = t2.Users_Id
)
select Request_at as [Day],
cast(sum(case when [Status] != 'completed' then 1 else 0 end) as float)/cast(count(*) as float) as cancellationRate
from joined_cte
where Banned = 'No'
group by Request_at





