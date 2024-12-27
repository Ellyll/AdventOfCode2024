FROM mcr.microsoft.com/dotnet/runtime:8.0 AS base
USER $APP_UID
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build
ARG BUILD_CONFIGURATION=Release
WORKDIR /src
COPY ["AdventOfCode2024.Day05/AdventOfCode2024.Day05.fsproj", "AdventOfCode2024.Day05/"]
RUN dotnet restore "AdventOfCode2024.Day05/AdventOfCode2024.Day05.fsproj"
COPY . .
WORKDIR "/src/AdventOfCode2024.Day05"
RUN dotnet build "AdventOfCode2024.Day05.fsproj" -c $BUILD_CONFIGURATION -o /app/build

FROM build AS publish
ARG BUILD_CONFIGURATION=Release
RUN dotnet publish "AdventOfCode2024.Day05.fsproj" -c $BUILD_CONFIGURATION -o /app/publish /p:UseAppHost=false

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "AdventOfCode2024.Day05.dll"]
