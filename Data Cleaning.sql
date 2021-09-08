/*

Cleaning Data using SQL Queries


*/

use ORG

-- View Data

Select *
From NashvilleHousing

-- Standardize Date Format

Select SaleDate, CONVERT(Date,SaleDate) from NashvilleHousing	-- Format we want it in, now lets update it

Update NashvilleHousing
Set SaleDate = CONVERT(Date,SaleDate)	-- Didnt work we can use alter table to add new column

Alter Table NashvilleHousing 
Add SaleDateConverted Date

Update NashvilleHousing
Set SaleDateConverted = CONVERT(Date,SaleDate) -- Let's view the new column

Select SaleDateConverted from NashvilleHousing

-- Populate Property Address Data where Address is null if ParcelID matches

select PropertyAddress,ParcelID
from NashvilleHousing where PropertyAddress is null -- Great now lets inner join and match them

select a.UniqueID,a.PropertyAddress, a.ParcelID, b.ParcelID, b.PropertyAddress, ISNULL(a.PropertyAddress,b.PropertyAddress)
from NashvilleHousing a inner join NashvilleHousing b on a.ParcelID = b.ParcelID and a.[UniqueID ]<>b.[UniqueID ] where a.PropertyAddress is null -- Let's create that new column

Update a
set PropertyAddress = ISNULL(a.PropertyAddress,b.PropertyAddress)
from NashvilleHousing a
inner join NashvilleHousing b on a.ParcelID = b.ParcelID and a.[UniqueID ]<>b.[UniqueID ] where a.PropertyAddress is null

-- Lets view if null still exists for the same parcelIDs

select a.UniqueID,a.PropertyAddress, a.ParcelID, b.ParcelID, b.PropertyAddress, ISNULL(a.PropertyAddress,b.PropertyAddress)
from NashvilleHousing a inner join NashvilleHousing b on a.ParcelID = b.ParcelID and a.[UniqueID ]<>b.[UniqueID ] where a.PropertyAddress is null -- GREAT! It's empty


-- Can we split the PropertyAddress to (Address, City, State)
select propertyaddress from NashvilleHousing  -- The format for all addresses are the same i.e. Address, City

select substring(propertyaddress, 1, CHARINDEX(',',propertyaddress,1)-1) as home_Address,
substring(propertyaddress, CHARINDEX(',',propertyaddress,1)+1, LEN(propertyaddress)) as city_Address
from NashvilleHousing

-- Let's alter the table
Alter table NashvilleHousing
Add HouseAddress Nvarchar(255)

Update NashvilleHousing
set HouseAddress = substring(propertyaddress, 1, CHARINDEX(',',propertyaddress,1)-1)

Alter table NashvilleHousing
Add CityAddress Nvarchar(255)

Update NashvilleHousing
set CityAddress = substring(propertyaddress, CHARINDEX(',',propertyaddress,1)+1, LEN(propertyaddress))

-- Lets look at the table now

select * from NashvilleHousing

-- Lets do the same for Owner Address by using PARSENAME -- Parsename looks for '.' and uses that to separate -- We replace our ',' with '.' and then parse!

Select
PARSENAME(REPLACE(OwnerAddress, ',', '.') , 3)
,PARSENAME(REPLACE(OwnerAddress, ',', '.') , 2)
,PARSENAME(REPLACE(OwnerAddress, ',', '.') , 1)
From NashvilleHousing

ALTER TABLE NashvilleHousing
Add OwnerHouseAddress Nvarchar(255);

Update NashvilleHousing
SET OwnerHouseAddress = PARSENAME(REPLACE(OwnerAddress, ',', '.') , 3)

ALTER TABLE NashvilleHousing
Add OwnerCityAddress Nvarchar(255);

Update NashvilleHousing
SET OwnerCityAddress = PARSENAME(REPLACE(OwnerAddress, ',', '.') , 2)


ALTER TABLE NashvilleHousing
Add OwnerStateAddress Nvarchar(255);

Update NashvilleHousing
SET OwnerStateAddress = PARSENAME(REPLACE(OwnerAddress, ',', '.') , 1)

-- Lets view the table

select * from NashvilleHousing

-- SoldAsVacant has inconsistent data - Yes or No but also Y or N -- We will make it Yes or No for all

select distinct SoldAsVacant, count(SoldAsVacant) as count_vacant from NashvilleHousing
group by SoldAsVacant order by count_vacant

-- Lets try changing the Y and N to Yes and No

select SoldAsVacant,
CASE When SoldAsVacant = 'Y' then 'Yes'
	When SoldAsVacant = 'N' then 'No'
	Else SoldAsVacant
	END
from NashvilleHousing

-- Lets update this table

Update NashvilleHousing
SET SoldAsVacant = CASE When SoldAsVacant = 'Y' then 'Yes'
						When SoldAsVacant = 'N' then 'No'
						Else SoldAsVacant
						END
from NashvilleHousing

-- Run the count code again and we see the conversion is successful

-- Let's delete columns we dont need anymore

ALTER TABLE NashvilleHousing
DROP COLUMN OwnerAddress, TaxDistrict, PropertyAddress, SaleDate

Select *
From NashvilleHousing

-- Remove duplicates assuming uniqueid is different

SELECT ParcelID, 
    SalePrice, 
    SaleDateConverted,
	LegalReference,
    COUNT(*) AS CNT
FROM NashvilleHousing
GROUP BY ParcelID, 
    SalePrice, 
    SaleDateConverted,
	LegalReference
HAVING COUNT(*) > 1; -- 104 rows duplicate


select * from NashvilleHousing
where [UniqueID ] not in (
SELECT max([UniqueID ])
FROM NashvilleHousing
GROUP BY ParcelID, 
    SalePrice, 
    SaleDateConverted,
	LegalReference
)	-- get the max unique id to delete

Delete from NashvilleHousing
where [UniqueID ] not in (
SELECT max([UniqueID ])
FROM NashvilleHousing
GROUP BY ParcelID, 
    SalePrice, 
    SaleDateConverted,
	LegalReference
)  -- 104 rows removed


-- Cleaned Data

select * from NashvilleHousing
