dofile("TSP_Data.lua")
dofile("Graphics.lua")
dofile("Bitmap.lua")
dofile("GA_Common.lua")

local POPULATION_SIZE       = 1500
local CROSSOVER_RATE        = 0.6
local MAX_GENERATIONS       = 600

local IMAGE_WIDTH           = 1280
local IMAGE_HEIGHT          = 720
local WRITE_FRAMES          = false

-- recalculatable and depending on other params in the TSP description
local CHROMOSOME_LENGTH     = false     -- this will be the number of cities
local INVERSION_RATE        = false     -- this will be set inverse of the CHROMOSOME_LENGTH

local function Min(a, b) return (not a or b < a) and b or a end
local function Max(a, b) return (not a or b > a) and b or a end

function table.min(t, functor)
	local min, min_i
	if functor then
		local min_value
		for i = 1, #t do
			local value = functor(t[i])
			if value and (not min_value or value < min_value) then
				min, min_value, min_i = t[i], value, i
			end
		end
	else
		min, min_i = t[1], 1
		for i = 2, #t do
			local value = t[i]
			if value < min then
				min, min_i = value, i
			end
		end
	end
	return min, min_i
end

function table.copy(t, deep, filter)
	if type(t) ~= "table" then
		assert(false, "Attept to table.copy a var of type " .. type(t))
		return {}
	end	

	if type(deep) == "number" then
		deep = deep > 1 and deep - 1
	end
	
	local meta = getmetatable(t)
	if meta then
		local __copy = rawget(meta, "__copy")
		if __copy then
			return __copy(t)
		elseif type(t.class) == "string" then
			assert(false, "Attept to table.copy an object of class " .. t.class)
			return {}
		end
	end
	local copy = {}
	for k, v in pairs(t) do
		if deep then
			if type(k) == "table" then k = table.copy(k, deep) end
			if type(v) == "table" then v = table.copy(v, deep) end
		end
		if not filter or filter(k, v) then
			copy[k] = v
		end
	end
	return copy
end

function table.find(array, field, value)
	if not array then return end
	if value == nil then
		value = field
		for i = 1, #array do
			if value == array[i] then return i end
		end
	else
		for i = 1, #array do
			if value == array[i][field] then return i end
		end
	end
end

local function GenerateRandomPermutation(cities)
  local avail = {}
  for city = 1, cities do
    avail[city] = city
  end
  
  local perm = {}
  for city = 1, cities do
    local idx = math.random(1, #avail)
    perm[city] = avail[idx]
    avail[idx] = avail[#avail]
    table.remove(avail)
  end
  
  return perm
end

local function PermutationDistance(tour, TSP)
  local dist = 0.0
  local prev_city = tour[#tour]
  for _, city in ipairs(tour) do
    dist = dist + TSP.dist[prev_city][city]
    prev_city = city
  end
  
  return dist
end

local function PermutationToList(perm)
  return "{" .. table.concat(perm, ",") .. "}"
end

local function GenerateInitPopulation(size, cities)
  local population = { crossovers = 0, inversions = 0 }
  while #population < size do
    table.insert(population, { tour = GenerateRandomPermutation(cities) })
  end
  
  return population
end

local function EvaluatePopulation(population, map, old_pop, gen)
  local total_distance, min_distance, max_distance = 0.0
  for _, individual in ipairs(population) do
    local distance = PermutationDistance(individual.tour, map)
    individual.distance = distance
    min_distance = Min(min_distance, distance)
    max_distance = Max(max_distance, distance)
    total_distance = total_distance + distance
  end
  population.total_distance = total_distance
  population.avg_distance = total_distance / #population
  population.min_distance, population.max_distance = min_distance, max_distance
  if old_pop then
    population.interim_performance = (old_pop.interim_performance * (gen - 1) + min_distance) / gen
  else
    population.interim_performance = min_distance
  end
  
  local total_fitness, min_fitness, max_fitness = 0.0
  for _, individual in ipairs(population) do
    local fitness = max_distance + 1.0 - individual.distance
    individual.fitness = fitness
    min_fitness = Min(min_fitness, fitness)
    max_fitness = Max(max_fitness, fitness)
    individual.fitness = fitness
    individual.part_total_fitness = total_fitness
    total_fitness = total_fitness + fitness
  end  
  population[0] = { fitness = 0.0, part_total_fitness = 0.0 }
  population.total_fitness = total_fitness
  population.avg_fitness = total_fitness / #population
  population.min_fitness, population.max_fitness = min_fitness, max_fitness
end

local function PlotPopulation(pop, funcs, generation)
  funcs["Min"][generation] = { x = generation, y = pop.min_distance }
  funcs["Max"][generation] = { x = generation, y = pop.max_distance }
  funcs["Avg"][generation] = { x = generation, y = pop.avg_distance }
  funcs["Interim"][generation] = { x = generation, y = pop.interim_performance }
end

local function RouletteWheelSelect(population)
  local slot = math.random() * population.total_fitness
  if slot <= 0 then
    return 1
  elseif slot >= population.total_fitness then
    return #population
  end
  
  local left, right = 1, #population
  while left + 1 < right do
    local middle = (left + right) // 2
    local part_total = population[middle].part_total_fitness
    if slot == part_total then
      return middle
    elseif slot < part_total then
      right = middle
    else
      left = middle
    end
  end
  
  return (slot < population[left].part_total_fitness + population[left].fitness) and left or right
end

function CrossoverPMX(mate1, mate2, crossover_length)
  local offspring1 = { tour = table.copy(mate1.tour) }
  local offspring2 = { tour = table.copy(mate2.tour) }
  local crossovers = 0
  
  if FlipCoin(CROSSOVER_RATE) then
    local tour1, tour2 = mate1.tour, mate2.tour
    local length = #tour1
    local xsite1 = math.random(1, length)
    local xsite2 = xsite1 + math.random(1, crossover_length or length)
    xsite2 = (xsite2 > length) and (xsite2 - length) or xsite2
    local xsite_end = (xsite2 < length) and (xsite2 + 1) or 1
    if xsite1 ~= xsite2 and xsite1 ~= xsite_end then
      local offtour1, offtour2 = offspring1.tour, offspring2.tour
      while xsite1 ~= xsite_end do
        local city1, city2 = tour1[xsite1], tour2[xsite1]
        local idx1, idx2 = table.find(offtour1, city2), table.find(offtour2, city1)
        offtour1[xsite1], offtour1[idx1] = offtour1[idx1], offtour1[xsite1]
        offtour2[xsite1], offtour2[idx2] = offtour2[idx2], offtour2[xsite1]
        xsite1 = (xsite1 < length) and (xsite1 + 1) or 1
      end
      crossovers = 1
    end
  end
  
  return offspring1, offspring2, crossovers
end

local function CompactZeros(array, pos1, pos2)
  local length = #array
  local non_zeroes = (pos1 < pos2) and (length - (pos2 - pos1 + 1)) or (pos1 - pos2 - 1)
  local pos = (pos2 < length) and (pos2 + 1) or 1
  local idx = pos
  while array[idx] ~= 0 do
    idx = (idx < length) and (idx + 1) or 1
  end
  while array[idx] == 0 do
    idx = (idx < length) and (idx + 1) or 1
  end
  while non_zeroes > 0 do
    if array[pos] == 0 then
      array[pos], array[idx] = array[idx], 0
      while array[idx] == 0 do
        idx = (idx < length) and (idx + 1) or 1
      end
    end
    pos = (pos < length) and (pos + 1) or 1
    non_zeroes = non_zeroes - 1
  end
end

function CrossoverOX(mate1, mate2, crossover_length)
  local offspring1 = { tour = table.copy(mate1.tour) }
  local offspring2 = { tour = table.copy(mate2.tour) }
  local crossovers = 0
  
  if FlipCoin(CROSSOVER_RATE) then
    local tour1, tour2 = mate1.tour, mate2.tour
    local length = #tour1
    local xsite1 = math.random(1, length)
    local xsite2 = xsite1 + math.random(1, crossover_length or length)
    xsite2 = (xsite2 > length) and (xsite2 - length) or xsite2
    local xsite_end = (xsite2 < length) and (xsite2 + 1) or 1
    if xsite1 ~= xsite2 and xsite1 ~= xsite_end then
      local hole1, hole2 = {}, {}
      local xsite = xsite1
      repeat
        hole2[tour1[xsite]], hole1[tour2[xsite]] = true, true
        xsite = (xsite < length) and (xsite + 1) or 1
      until xsite == xsite_end
      local offtour1, offtour2 = offspring1.tour, offspring2.tour
      for pos = 1, length do
        local city1, city2 = tour1[pos], tour2[pos]
        offtour1[pos] = hole1[city1] and 0 or city1
        offtour2[pos] = hole2[city2] and 0 or city2
      end
      CompactZeros(offtour1, xsite1, xsite2)
      CompactZeros(offtour2, xsite1, xsite2)
      local xsite = xsite1
      repeat
        offtour1[xsite], offtour2[xsite] = tour2[xsite], tour1[xsite]
        xsite = (xsite < length) and (xsite + 1) or 1
      until xsite == xsite_end
      crossovers = 1
    end
  end
  
  return offspring1, offspring2, crossovers
end

local function TransferCycle(tour1, tour2, offtour)
  -- find the cycle
  local idx1 = 1
  while not offtour[idx1] do
    offtour[idx1] = tour1[idx1]
    idx1 = table.find(tour1, tour2[idx1])
  end
  
  -- transfer the rest from tour2
  for idx = 1, #tour1 do
    offtour[idx] = offtour[idx] or tour2[idx]
  end
end

function CrossoverCX(mate1, mate2)
  local offspring1 = { tour = {} }
  local offspring2 = { tour = {} }
  local crossovers = 0
  
  if FlipCoin(CROSSOVER_RATE) then
    local tour1, tour2 = mate1.tour, mate2.tour
    local offtour1, offtour2 = offspring1.tour, offspring2.tour
    TransferCycle(tour1, tour2, offtour1)
    TransferCycle(tour2, tour1, offtour2)
    crossovers = 1
  else
    offspring1.tour = table.copy(mate1.tour)
    offspring2.tour = table.copy(mate2.tour)
  end
  
  return offspring1, offspring2, crossovers
end

local function Inverse(offspring, inversion_length)
  local inversions = 0
  
  if FlipCoin(INVERSION_RATE) then
    local tour = offspring.tour
    local len = #tour
    local pos1 = math.random(1, len)
    local pos2 = pos1 + math.random(1, inversion_length)
    while pos1 < pos2 do
      local idx1 = (pos1 <= len) and pos1 or (pos1 - len)
      local idx2 = (pos2 <= len) and pos2 or (pos2 - len)
      tour[idx1], tour[idx2] = tour[idx2], tour[idx1]
      pos1, pos2 = pos1 + 1, pos2 - 1
    end
    inversions = 1
  end

  return inversions
end

local function PrecalculateCityDistances(TSP)
  if TSP.dist then return end
  
  local dist = {}
  for i = 1, #TSP do
		dist[i] = {}
  end
  
  for i = 1, #TSP do
		for k = i + 1, #TSP do
			local length = math.floor(Euc2D(TSP[i],TSP[k]) + 0.5)
			dist[i][k], dist[k][i] = length, length
		end
	end
  
  TSP.dist = dist
end

local function CreateGraphs(filename, stats, name, optimal, crossover_type, size_x, title_text, time_text, info, info2, info3)
  local write_frames = WRITE_FRAMES and name and string.format("TSP/%s_%s_%s", filename, name, crossover_type)
  filename = string.format("TSP/%s_%s_%s.bmp", filename, name or "Multi", crossover_type)
  print(string.format("Writing '%s'...", filename))
  
  local graphs
  if name then
    graphs = { funcs = {}, name_x = stats.name_x, name_y = stats.name_y }
    graphs.funcs[name] = stats.funcs[name]
  else
    graphs = stats
  end
  graphs.funcs[string.format("Optimal=%d", optimal)] = { {x = 0, y = optimal}, {x = size_x, y = optimal}, color = {128, 128, 128} }
  
  local bmp = Bitmap.new(IMAGE_WIDTH, IMAGE_HEIGHT, {0, 0, 0})
  local top = 5
  local tw, th = bmp:MeasureText(time_text)
  bmp:DrawText(IMAGE_WIDTH - tw - 5, top, time_text, {128, 128, 128})
  tw, th = bmp:MeasureText(title_text)
  bmp:DrawText((IMAGE_WIDTH - tw) // 2, top, title_text, {128, 128, 128})
  top = top + th + 5
  tw, th = bmp:MeasureText(info)
  bmp:DrawText((IMAGE_WIDTH - tw) // 2, top, info, {128, 128, 128})
  top = top + th + 5
  tw, th = bmp:MeasureText(info2)
  bmp:DrawText((IMAGE_WIDTH - tw) // 2, top, info2, {128, 128, 128})
  tw, th = bmp:MeasureText(info3)
  bmp:DrawText((IMAGE_WIDTH - tw) // 2, IMAGE_HEIGHT - 20, info3, {128, 128, 128})
  DrawGraphs(bmp, graphs, nil, nil, "int x", "int y", "skip KP", optimal, write_frames, name, WRITE_FRAMES)
  bmp:WriteBMP(filename)
end

local function DrawTour(filename, TSP, descr, crossover_type, title_text, info, info2, frame)
  filename = string.format("TSP/%s_%s_%s.bmp", filename, crossover_type, frame and string.format("%04d", frame) or "")
  print(string.format("Writing '%s'...", filename))
  
  local graphs = { funcs = { ["City Tour"] = { color = {0, 255, 0} } }, name_x = "X", name_y = "Y" }
  local tour_graph = graphs.funcs["City Tour"]
  for idx, city in ipairs(descr.tour) do
    local pt = TSP[city]
    tour_graph[idx] = { x = pt[1], y = pt[2] }
  end
  table.insert(tour_graph, table.copy(tour_graph[1]))

  local bmp = Bitmap.new(IMAGE_WIDTH, IMAGE_HEIGHT, {0, 0, 0})
  DrawGraphs(bmp, graphs, nil, nil, "int x")
  local top = 5
  local tw, th = bmp:MeasureText(title_text)
  bmp:DrawText((IMAGE_WIDTH - tw) // 2, top, title_text, {128, 128, 128})
  top = top + th + 5
  if info then
    tw, th = bmp:MeasureText(info)
    bmp:DrawText((IMAGE_WIDTH - tw) // 2, top, info, {128, 128, 128})
    top = top + th + 5
  end
  if info2 then
    tw, th = bmp:MeasureText(info2)
    bmp:DrawText((IMAGE_WIDTH - tw) // 2, top, info2, {128, 128, 128})
  end
  bmp:WriteBMP(filename)
end

local function RunTSP(TSP, crossover_type, crossover_length_percents, inversion_length_percents, filename)
  start_clock = os.clock()
  
  if not TSP.dist then
    PrecalculateCityDistances(TSP)
    print(string.format("Precalculated city distances: %ss", os.clock() - start_clock))
  end
  
  local cities = #TSP
  local crossover_length = crossover_length_percents * cities // 100
  local inversion_length = inversion_length_percents * cities // 100
  CHROMOSOME_LENGTH = cities
  INVERSION_RATE = 1.0 / CHROMOSOME_LENGTH
  local Crossover = _ENV._G["Crossover" .. crossover_type]
  print(string.format("Crossover %s length: %d, Inversion length: %d", crossover_type, crossover_length, inversion_length))
  local title_text = string.format("%s: %d%% Crossver Length, %d%% Inversion Length", TSP.name, crossover_length_percents, inversion_length_percents)
  
  local stats =
  {
    funcs =
    {
      ["Max"] = { color = {0, 0, 255} },
      ["Min"] = { color = {0, 255, 0} },
      ["Avg"] = { color = {255, 0, 0} },
      ["Interim"] = { color = {0, 255, 0} },
    },
    name_x = "Generation #",
    name_y = "Tour Distance",
  }
  
  local init_pop = GenerateInitPopulation(POPULATION_SIZE, cities)
  EvaluatePopulation(init_pop, TSP)
  PlotPopulation(init_pop, stats.funcs, 1)
  local best_tour = table.min(init_pop, function(individual) return individual.distance end)
  best_tour.generation = 1
  local first_gen_found
  local pop = init_pop
  
  local function GetInfoTexts()
    local info = string.format("Population: %d, Generation found: %s", #pop, first_gen and tostring(first_gen) or ":(")
    local info2 = string.format("Best found: %d(%.2f%% Optimal) at Generation: %d", best_tour.distance, 100.0 * TSP.OptimalTourDistance / best_tour.distance, best_tour.generation)
    local info3 = string.format("%s Crossover type, Crossovers: %d, Inversions: %d", crossover_type, pop.crossovers, pop.inversions)
    
    return info, info2, info3
  end
  
  local frame = 1
  if WRITE_FRAMES and filename then
    local _, info2, info3 = GetInfoTexts()
    DrawTour(filename .. "_Tour", TSP, best_tour, crossover_type, title_text, info2, info3, 1)
  end  
  for gen = 2, MAX_GENERATIONS do
    if gen % 100 == 0 then
      print(string.format("%ss: Generation # %d/%d", os.clock() - start_clock, gen, MAX_GENERATIONS))
    end
    local new_pop = { crossovers = pop.crossovers, inversions = pop.inversions }
    while #new_pop < POPULATION_SIZE do
      local idx1 = RouletteWheelSelect(pop)
      local idx2 = RouletteWheelSelect(pop)
      local offspring1, offspring2, crossovers
      if crossover_length > 0 then
        offspring1, offspring2, crossovers = Crossover(pop[idx1], pop[idx2], crossover_length)
        new_pop.crossovers = new_pop.crossovers + crossovers
      else
        offspring1 = { tour = table.copy(pop[idx1].tour) }
        offspring2 = { tour = table.copy(pop[idx2].tour) }
      end
      if inversion_length > 0 then
        local inversions1 = Inverse(offspring1, inversion_length)
        table.insert(new_pop, offspring1)
        new_pop.inversions = new_pop.inversions + inversions1
        if #new_pop < POPULATION_SIZE then          -- in case population size is odd number
          local inversions2 = Inverse(offspring2, inversion_length)
          table.insert(new_pop, offspring2)
          new_pop.inversions = new_pop.inversions + inversions2
        end
      else
        table.insert(new_pop, offspring1)
        if #new_pop < POPULATION_SIZE then          -- in case population size is odd number
          table.insert(new_pop, offspring2)
        end
      end
    end
    EvaluatePopulation(new_pop, TSP, pop, gen)
    pop = new_pop
    PlotPopulation(pop, stats.funcs, gen)
    if math.abs(pop.min_fitness - TSP.OptimalTourDistance) < 0.0001 then
      first_gen_found = gen
    end
    if pop.min_distance < best_tour.distance then
      best_tour = table.min(pop, function(individual) return individual.distance end)
      best_tour.generation = gen
    end
    if WRITE_FRAMES and filename and (gen % WRITE_FRAMES == 0 or gen == MAX_GENERATIONS) then
      local _, info2, info3 = GetInfoTexts()
      frame = frame + 1
      DrawTour(filename .. "_Tour", TSP, best_tour, crossover_type, title_text, info2, info3, frame)
    end
  end

  local time = os.clock() - start_clock
  local time_text = string.format("Time (Lua 5.3): %ss", time)
  print(time_text)
  
  if filename then
    local info, info2, info3 = GetInfoTexts()
    CreateGraphs(filename, stats, "Min", TSP.OptimalTourDistance, crossover_type, MAX_GENERATIONS, title_text, time_text, info, info2, info3)
    CreateGraphs(filename, stats, "Interim", TSP.OptimalTourDistance, crossover_type, MAX_GENERATIONS, title_text, time_text, info, info2, info3)
    DrawTour(filename .. "_Tour", TSP, best_tour, crossover_type, title_text, info2, info3)
  end
  
  return best_tour
end

local s_Tests =
{
  ["Berlin52_OX_50_Inversion"] =
  {
    TSP_name = "Berlin52", crossover_type = "OX",
    title = "Inversion Length VS Tour Distance",
    name_y = "Tour Length",
    color = {0, 255, 0},
    crossover = {50, 50, 1}, inversion = {10, 100, 10},
    size_x = 100,
    filename = "Berlin52_OX_50_Inversion",
  },
  ["Berlin52_PMX_60_Inversion"] =
  {
    TSP_name = "Berlin52", crossover_type = "PMX",
    title = "Inversion Length VS Tour Distance",
    name_y = "Tour Length",
    color = {0, 255, 0},
    crossover = {60, 60, 1}, inversion = {10, 100, 10},
    size_x = 100,
    filename = "Berlin52_PMX_60_Inversion",
  },
  ["Berlin52_PMX_Crossover_80"] =
  {
    TSP_name = "Berlin52", crossover_type = "PMX",
    title = "Inversion Length VS Tour Distance",
    name_y = "Tour Length",
    color = {0, 255, 0},
    crossover = {80, 80, 10}, inversion = {10, 100, 10},
    size_x = 100,
    filename = "Berlin52_PMX_Crossover_80",
  },
  ["Berlin52_PMX"] =
  {
    TSP_name = "Berlin52", crossover_type = "PMX",
    title = "Search for Best Crossover & Inversion",
    name_y = "Tour Length",
    color = {0, 255, 0},
    crossover = {10, 100, 10}, inversion = {10, 100, 10},
    size_x = 100,
    multi_name = "XLen%d%%",
    filename = "Berlin52",
  },
  ["Berlin52_OX"] =
  {
    TSP_name = "Berlin52", crossover_type = "OX",
    title = "Search for Best Crossover & Inversion",
    name_y = "Tour Length",
    color = {0, 255, 0},
    crossover = {10, 100, 10}, inversion = {10, 100, 10},
    size_x = 100,
    multi_name = "XLen%d%%",
    filename = "Berlin52",
  },
  ["Berlin52_CX_5_50"] =
  {
    TSP_name = "Berlin52", crossover_type = "CX",
    title = "Search for Best Inversion(Crossover does not matter)",
    name_y = "Tour Length",
    color = {0, 255, 0},
    crossover = {0, 100, 100}, inversion = {5, 50, 5},
    size_x = 100,
    multi_name = "XLen%d%%",
    filename = "Berlin52_5_50",
  },
  ["Berlin52_CX_55_100"] =
  {
    TSP_name = "Berlin52", crossover_type = "CX",
    title = "Search for Best Inversion(Crossover does not matter)",
    name_y = "Tour Length",
    color = {0, 255, 0},
    crossover = {0, 100, 100}, inversion = {55, 100, 5},
    size_x = 100,
    multi_name = "XLen%d%%",
    filename = "Berlin52_55_100",
  },
}

local function RunTest(test_name)
  local start_clock = os.clock()
  
  local colors =
  {
    {0, 255, 0}, {255, 0, 0}, {0, 0, 255},
    {255, 255, 0}, {255, 0, 255}, {0, 255, 255},
    {255, 255, 255}, {128, 128, 128}, {192, 128, 255}, {128, 255, 192},
  }
  
  local test = s_Tests[test_name]
  local TSP = _ENV._G[test.TSP_name]
  local multi_crossovers = (test.crossover[1] ~= test.crossover[2]) and (test.inversion[1] ~= test.inversion[2])
  local graphs =
  {
      funcs = {},
      name_x = (test.crossover[1] == test.crossover[2] or multi_crossovers) and "Inversion Length %" or "Crossover Length %",
      name_y = test.name_y,
  }
  if multi_crossovers then
    local idx = 1
    for crossover_length = test.crossover[1], test.crossover[2], test.crossover[3] do
      graphs.funcs[string.format(test.multi_name, crossover_length)] = {color = colors[idx]}
      idx = idx + 1
    end
  else
    graphs.funcs["Crossover Length"] = {color = test.color}
    graphs.funcs["Inversion Length"] = {color = test.color}
  end
  
  local min
  for crossover_length = test.crossover[1], test.crossover[2], test.crossover[3] do
    local name = multi_crossovers and string.format(test.multi_name, crossover_length)
    for inversion_length = test.inversion[1], test.inversion[2], test.inversion[3] do
      math.randomseed(1234)
      local best = RunTSP(TSP, test.crossover_type, crossover_length, inversion_length)
      if multi_crossovers then
        table.insert(graphs.funcs[name], {x = inversion_length, y = best.distance})
      else
        table.insert(graphs.funcs["Crossover Length"], {x = crossover_length, y = best.distance})
        table.insert(graphs.funcs["Inversion Length"], {x = inversion_length, y = best.distance})
      end
      if not min or min.distance > best.distance then
        min = best
        min.crossover_length = crossover_length
        min.inversion_length = inversion_length
      elseif min.distance == best.distance then
        min.others = min.others or {}
        table.insert(min.others, {crossover_length = crossover_length, inversion_length = inversion_length})
      end
    end
  end
  
  local time = os.clock() - start_clock
  local time_text = string.format("Time (Lua 5.3): %ss", time)
  local optimal = 100.0 * TSP.OptimalTourDistance / min.distance
  local info
  if test.crossover[1] == test.crossover[2] then
    info = string.format("Best: %d(%.2f%% Optimal), Crossover Length: %d%%", min.distance, optimal, min.crossover_length)
    if min.others then
      print("Other optimal inversion lengths:")
      for _, other in ipairs(min.others) do
        print(string.format("%d%%", other.inversion_length))
      end
    end
  elseif test.inversion[1] == test.inversion[2] then
    info = string.format("Best: %d(%.2f%% Optimal), Inversion length: %d%%", min.distance, optimal, min.inversion_length)
    if min.others then
      print("Other optimal crossover lengths:")
      for _, other in ipairs(min.others) do
        print(string.format("%d%%", other.crossoover_length))
      end
    end
  else
    info = string.format("Best: %d(%.2f%% Optimal), Crossover length: %d%%, Inversion Length: %d%%", min.distance, optimal, min.crossover_length, min.inversion_length)
    if min.others then
      print("Other optimal crossover/inversion lengths:")
      for _, other in ipairs(min.others) do
        print(string.format("%d%%/%d%%", other.crossover_length, other.inversion_length))
      end
    end
  end
  if multi_crossovers then
    CreateGraphs(test.filename, graphs, false, Berlin52.OptimalTourDistance, test.crossover_type, test.size_x, test.title, time_text, info, "", "")    
  else
    local name = (test.crossover[1] == test.crossover[2]) and "Inversion Length" or "Crossover Length"
    CreateGraphs(test.filename, graphs, name, Berlin52.OptimalTourDistance, test.crossover_type, test.size_x, test.title, time_text, info, "", "")
  end
end

--RunTest("Berlin52_PMX")
--RunTest("Berlin52_OX")
--RunTest("Berlin52_CX_5_50")
--RunTest("Berlin52_CX_55_100")

RunTSP(Berlin52, "PMX", 80, 30, "Berlin52_PMX_80_30")
--RunTSP(Berlin52, "OX", 10, 70, "Berlin52_OX_10_70")
--RunTSP(Berlin52, "CX", 100, 40, "Berlin52_CX_100_40")
--RunTSP(Berlin52, "CX", 0, 60, "Berlin52_CX_0_60")
--RunTSP(City76)
--RunTSP(City1002)