{-
def check_account_balance(amount, month):
	if amount < 0:
		raise Exception('We went negative at month_num [{}]'.format(month))

def elapse_time(years, annual_rate_of_return, amount_to_invest, amount_to_vacation):
	table = [('month/year', 'extra', 'investments', 'vacations')]
	investments = 0
	vacations = 0
	extra = 0
	for month_num in range(years * 12):
		month = month_num % 12
		year = month_num // 12
		for fn in income_and_expenses:
			extra += fn(month, year)
		check_account_balance(extra, month_num)

		extra -= amount_to_invest
		investments += amount_to_invest
		investments += (investments * (annual_rate_of_return / 12))
		check_account_balance(extra, month_num)

		extra -= amount_to_vacation
		vacations += amount_to_vacation
		check_account_balance(extra, month_num)

		table.append((str(month+1)+'/'+str(year), extra, investments, vacations))
-}
module Account exposing (..)


type FontSize
  = Small
  | Medium
  | Large
