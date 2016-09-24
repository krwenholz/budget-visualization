{-
def google(month, year):
	salary = (3200 / 14) * 30
	if month in (2, 10):
		return salary + 10000
	return salary

def lakeside(month, year):
	if month in (8, 9, 10):
		return 600
	return 0

def cascade(month, year):
	if month in (10, 11, 0, 1, 2, 3, 4):
		return 300
	return 0

def bush_school(month, year):
	if month in (5, 6, 7, 8):
		return 1000
	return 1600

def monthly_expenses(month, year):
	credit_card_expenses = 3100
	return - credit_card_expenses

def housing(month, year):
	return - 1570

def car_payment(month, year):
	if year < 2 and month < 3:
		return - 400
	return 0

def charitable_giving(month, year):
  return -400
-}
module IncomeEvent exposing (..)

type IncomeEvent = {
	
}
