{ pkgs }:

{
  brass = pkgs.fetchlfs { src = ../../../bin/brass.pill; };
  ivory = pkgs.fetchlfs { src = ../../../bin/ivory.pill; };
  solid = pkgs.fetchlfs { src = ../../../bin/solid.pill; };
}
