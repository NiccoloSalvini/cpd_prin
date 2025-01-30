import pandas as pd
import geopandas as gpd
import re

df = pd.read_csv("data/hotels_raw.csv")

prov = gpd.read_file("data/ProvCM01012024_g_WGS84.shp").to_crs(4326)


# Convert to GeoDataFrame
gdf = gpd.GeoDataFrame(
    df, 
    geometry=gpd.points_from_xy(df['coordinates.longitude'], df['coordinates.latitude']),
    crs="EPSG:4326" # WGS 84 coordinate system
)

joined = gpd.sjoin(gdf, prov)

# Many hotels not in Italy...but some just outside the shapefile limits
missing = df[~df["accommodationId"].isin(joined["accommodationId"].unique())]

to_be_added = missing[missing["details.address.last"].str.contains("Italia", na=False)]

# Extract the county code (two uppercase letters after a comma and space)
to_be_added['SIGLA'] = df['details.address.last'].str.extract(r', (\b[A-Z]{2}\b),')

prov_map = prov[['SIGLA', 'DEN_UTS']]

recovered = pd.merge(to_be_added, prov_map, on="SIGLA", how="left")

#joined.columns

results = pd.concat([joined[['accommodationId', 'coordinates.latitude', 'coordinates.longitude',
       'details.name', 'details.stars', 'details.address.first',
       'details.address.last', 'title','SIGLA', 'DEN_UTS']], recovered])

results.to_csv("data/hotels_prov.csv", index=False)



