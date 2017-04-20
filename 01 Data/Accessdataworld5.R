require(data.world)
conn <- data.world()
globalshipments <- query(conn, dataset="jlee/s-17-dv-project-5", type = "sql",query="SELECT * FROM `globalshipments.csv/globalshipments` LIMIT 10")

require(data.world)
conn <- data.world()
census <- query(conn, dataset = "jlee/s-17-dv-project-5", type = "sql", query = "SELECT * FROM `census-pop-sex.csv/census-pop-sex` LIMIT 10")



print(summary(globalshipments))
print(summary(census))
print(head(globalshipments))
print(head(census))
