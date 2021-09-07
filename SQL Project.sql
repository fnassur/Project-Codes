---- SQL Queries for Practice ----
use ORG
-- Retrive first name as alias from Worker
Select FIRST_NAME AS WORKER_NAME from Worker 

-- Lets get that name in upper case
Select UPPER(FIRST_NAME) AS WORKER_NAME FROM WORKER

-- What departments do we have in Worker Table
Select distinct DEPARTMENT FROM WORKER

-- Cut em short... first 3 characters of first name from Worker
Select substring(FIRST_NAME,1,3) as WORKER_NAME FROM WORKER 

-- Position of the alphabet 'a' in first column where name is Amitabh
Select CHARINDEX('a',FIRST_NAME COLLATE Latin1_General_CS_AS) from Worker where FIRST_NAME = 'Amitabh'

-- First name from Workers after removing white sapces from right side
select RTRIM(FIRST_NAME) FROM WORKER

-- First name from Workers after removing white spaces from left side
select LTRIM(FIRST_NAME) from Worker

-- Let's get the unique department values and print its length
Select distinct (len(department)) from worker

-- First name after replacing 'a' with 'A' from worker
select replace(first_name, 'a', 'A') from worker

-- Lets print out first name with last name as full name with a space between
select concat(first_name,' ', last_name) as FULL_NAME from Worker

-- Lets get all the details ordered by first name ascending
Select * from worker order by first_name asc

-- Lets get the details ordered by first name ascending and department descending
select * from worker order by first_name asc, department desc

-- Lets get the details of workers with names Vipul and Satish from worker
select * from worker where first_name in ('Vipul', 'Satish')

-- Lets exclude Vipul and Satish
select * from worker where first_name not in ('Vipul', 'Satish')

-- Get all admin details!
select * from worker where department = 'Admin'

-- Details of workers with first name containing 'a'
select * from worker where first_name like '%a%'

-- Details of workers with first name having 6 letters and ends with 'h'
select * from worker where first_name like '_____h'

-- Details of workers with salary between 100000 and 500000
select * from worker where salary between 100000 and 500000

-- Details of workers that have joined in feb 2020
select * from worker where year(joining_date) = 2020 and month(joining_date) = 2

-- Number of employees in Admin department
select count(*) from Worker where department = 'Admin'

-- Worker names with salary between 50000 and 100000
select concat(first_name,' ', last_name) as worker_name, salary from worker where salary between 50000 and 100000

-- Fetch number of workers in each department in descending order
select department,count(worker_id) as employee_count from worker group by department order by employee_count desc

-- Lets get workers who are also managers refer table worker and title
select distinct w.worker_id, w.first_name, t.worker_title from worker w, title t where w.worker_id = t.worker_ref_id and t.worker_title = 'Manager'

	--- alternate query ---
select distinct w.first_name, t.worker_title from worker w inner join title t on w.worker_id = t.worker_ref_id where t.worker_title = 'Manager'

-- Fetch duplicate records from title table - columns worker_title and affected_from
select count(*), worker_title, affected_from from title group by worker_title, affected_from having count(*)>1

-- Display odd rows
SELECT * FROM Worker WHERE worker_id %2 <> 0

-- Display even rows
select * from worker where worker_id %2=0

-- Clone a table
--select * into worker_clone from worker

-- Lets intersect the two tables
select * from worker 
intersect 
select * from worker_clone

-- Show records from table that the other table doesnt have
select * from worker
minus
select * from title

-- Show top 10 records of workers ordered by salary
select top 10 * from worker order by salary desc

-- Show 5th record of worker ordered by salary
select top 1 * from (SELECT DISTINCT TOP 5 *
 FROM Worker ORDER BY Salary desc) a order by a.salary asc

-- Show the above query in an alternative method
 select salary from worker w1 where 4  = (select count(distinct w2.salary) from worker w2 where w2.salary >= w1.salary)

-- Show employees with the same salary
 select w.first_name, w.salary from worker w, worker w1 where w.salary = w1.salary and w.worker_id != w1.worker_id

-- Show the second highest salary from workers
 select top 1 salary from (select distinct top 2  salary from worker order by salary desc) a order by salary asc

	-- alternate query
Select max(Salary) from Worker where Salary not in (Select max(Salary) from Worker);

-- Show a query result twice 
select * from worker w1 where w1.DEPARTMENT = 'HR'
union all
select * from worker w2 where w2.DEPARTMENT = 'HR'

-- Show intersecting records
select * from worker 
intersect
select * from worker_clone

-- Fetch first 50% of records from a table
select * from worker w1 where w1.WORKER_ID <= (select count(worker_id)/2 from worker w2)

-- Fetch departments that have less than 5 people in it
select count(*) as department_count, DEPARTMENT from worker group by department having count(*) <5

-- Show the last record of the table
select w1.* from worker w1 where w1.WORKER_ID = (select max(w2.worker_id) from worker w2)

-- Fetch last 5 records from table
select w1.* from worker w1 where w1.WORKER_ID > ((select max(w2.worker_id) from worker w2)-5)

-- Employee having highest salary in each department
select w1.first_name, w1.salary, w1.department from worker w1 where w1.salary = (select max(w2.salary)from worker w2 where w2.DEPARTMENT = 'HR')
union
select w1.first_name, w1.salary, w1.department from worker w1 where w1.salary = (select max(w2.salary)from worker w2 where w2.DEPARTMENT = 'Admin')
union
select w1.first_name, w1.salary, w1.department from worker w1 where w1.salary = (select max(w2.salary)from worker w2 where w2.DEPARTMENT = 'Account')

	-- Alternate better method

select w1.first_name, w1.salary, w1.department from (select max(salary) as max_sal, department from worker group by DEPARTMENT) as temp1 
Inner join worker w1 on 
temp1.DEPARTMENT = w1.DEPARTMENT and
temp1.max_sal = w1.salary

-- Fetch max 3 salaries
select distinct top 3 salary from worker order by SALARY desc

	-- Alternate method
	SELECT distinct Salary from worker a WHERE 3 >= (SELECT count(distinct Salary) from worker b WHERE a.Salary <= b.Salary) order by a.Salary desc;

-- Fetch min 3 salaries
select distinct top 3 salary from worker order by SALARY asc

	-- Alternate method
SELECT distinct Salary from worker a WHERE 3 >= (SELECT count(distinct Salary) from worker b WHERE a.Salary >= b.Salary) order by a.Salary desc;

-- Total salaries for each department
select sum(salary), department from worker group by department

-- Workers that earn the max salary

select w1.first_name, w1.salary from worker w1 where w1.salary = (select max(w2.salary) from worker w2)
