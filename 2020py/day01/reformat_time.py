from datetime import datetime, timedelta
import re
from typing import Annotated
from pydantic import BaseModel, PlainSerializer, PlainValidator

# format timedelta as HH:MM:SS
ReadableTimedelta = Annotated[
    timedelta,
    PlainSerializer(timedelta.__str__, when_used='json-unless-none'),
    PlainValidator(lambda s: timedelta(**re.match(r'(?P<hours>\d+):(?P<minutes>\d+):(?P<seconds>\d+)(.(?P<microseconds>\d+))?', s).groupdict()), )
    ]


class DayStats(BaseModel):
    started: datetime | None
    part1: ReadableTimedelta | None = None
    part2: ReadableTimedelta | None = None

# class DayStatsNew(BaseModel):
#     started: datetime | None

test = DayStats(started=datetime.now(), part1=timedelta(minutes=60), part2=timedelta(seconds=2))
print(test.model_dump_json())
test2 = DayStats.model_validate_json(test.model_dump_json())
print(test2.model_dump_json())