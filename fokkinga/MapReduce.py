"""Code related to
    "MapReduce - a two-page explanation for laymen" by M. Fokkinga."""
def merge(list1,list2):
    "Merge two sorted lists."
	result = []
	while list1 and list2:
		if list1[0]<=list2[0]:
			result.append(list1[0])
			list1 = list1[1:]
		else:
			result.append(list2[0])
			list2 = list2[1:]
	result = result + list1 + list2
	return result

def sort(ls):
    "Sort a list by mapping eacn element to a singleton, then folding with merge"
	return reduce(merge,
		      map(list, ls),
		      [])
