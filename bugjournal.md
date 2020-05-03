# Bug Journal

## 5-1-20
Travis is failing and I'm not sure why, it says there's something wrong with elm-format

Solution: Had to elm-format all of my files before deploying to Travis, as well as including tests for elm-test to pass

## 5-2-20
Moving ingredients wasn't working as intended, was reversed order because of how lists work

Solution: Switched the moveIngredientDown function logic with the moveIngredientUp one, easy enough!