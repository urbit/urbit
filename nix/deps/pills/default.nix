{ pkgs }:

{
  brass = pkgs.fetchgithublfs { src = ../../../bin/brass.pill; };
  ivory = pkgs.fetchgithublfs { src = ../../../bin/ivory.pill; };
  solid = pkgs.fetchgithublfs { src = ../../../bin/solid.pill; };
}
