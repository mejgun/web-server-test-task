#!/bin/bash
set -x 

./curl.sh user/create '{"name":"asd","1lastname":"dfd","login":"login1","password":"pass"}'
./curl.sh user/create '{"name":"asd","lastname":"dfd","login":"login2","password":"pass"}'
./curl.sh user/create '{"name":"authorname","lastname":"authorlastname","login":"authorlogin","password":"pass"}'
./curl.sh user/create '{"name":"asd","lastname":"dfd","photo":"iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAABmJLR0QA/wD/AP+gvaeTAAABM0lEQVRIieXVTS4EURQF4I9OkPgLa2BEb8BfYsoSmLECJKyiQ0ibsAqmLIAwwVzCiBlC2k8YeJWuVHR5VT2ROMnNTW6de86tW3mv+A+oYguXeApxiU2MtyPcjTo+8Nki3rGDrjLixznC2TgqarJbQDyJ7Vjxqvy15K1rLCvW+YPBcov6b6hgKYZ4VWDqbFzEGDy2YfAQY5CQJ2LIAVOpvlx0xBJzButIF7MfczXktxIGSc9KHuk6TDFZwmAy9N6li9k36Am5t4RBX8gfeaQ1zV3uFRDfT/Wt/sK1iAZeMRIhPup7/w0sxE5UC9McyD/VFRwGbi1WHIZxHxrrLUwqmpfiHYaKGMAsXoLAGebQH2Ie5+HZM2aKiieYxq3WV8ON71PcFgaxgVPNX+YJ1jHQrvjfxxfMl44mhMjC3wAAAABJRU5ErkJggg==","login":"login3","password":"pass"}'
