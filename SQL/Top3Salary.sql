/*
leetcode 185: Top 3 salaries
*/
create table Top3Salary
(
[department] varchar(10),
[name] varchar(10),
[salary] float
)

insert into Top3Salary
values ('a','jake',2),
('b','mark',27),
('a', 'henry', 46),
('a','micheal', 23),
('b', 'jone', 81),
('a', 'lucy', 73),
('a', 'lily', 92)

--select * from Top3Salary
;with tmp as
(
select *, ROW_NUMBER() over (partition by department order by salary desc) as RN
from Top3Salary
)

select department, [name] from tmp where RN <= 3 order by department