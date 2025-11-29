import json
import statistics
import matplotlib.pyplot as plt

with open("gadgets.json", 'r', encoding='utf-8') as f:
    gadgets = json.load(f)

for g in gadgets:
    g['price'] = float(g['price'])

gadgets_sorted = sorted(gadgets, key=lambda k: k['price'])

cheapest_price = gadgets_sorted[0]
most_expensive_price = gadgets_sorted[-1]
mean_price = statistics.mean(p['price'] for p in gadgets_sorted)

closest = min(gadgets, key=lambda k: abs(k['price'] - mean_price))

print(f"Najtanszy: {cheapest_price['title']} {cheapest_price['price']}")
print(f"Najdrozsy: {most_expensive_price['title']} {most_expensive_price['price']}")
print(f"Najbliżej średniej: {closest['title']} {closest['price']}")

five_cheapest = gadgets_sorted[:5]
names = [p['title'] for p in five_cheapest]
prices = [p['price'] for p in five_cheapest]

plt.figure(figsize=[10,5])
plt.bar(names, prices)
plt.title("5 najtanszych produktow")
plt.xlabel("produkt")
plt.ylabel("Cena (zł)")
plt.show()
