/-  *write
/+  *write, elem-to-react-json
|_  rum=rumor
++  grab
  |%
  ++  noun  rumor
  --
++  grow
  |%
  ++  noun  rum
  ++  json
    =,  enjs:format
    %+  frond  -.rum
    ?-  -.rum
        %collection
      %-  pairs 
      :~  [%coll s+col.rum]
          [%who (ship who.rum)]
          [%data (collection-build-to-json dat.rum)]
      ==
    ::
        %post
      %-  pairs 
      :~  [%coll s+col.rum]
          [%post s+pos.rum]
          [%who (ship who.rum)]
          [%data (post-build-to-json dat.rum)]
      ==
    ::
        %comments
      %-  pairs 
      :~  [%coll s+col.rum]
          [%post s+pos.rum]
          [%who (ship who.rum)]
          [%data (comment-build-to-json dat.rum)]
      ==
    ::
        %total
      %-  pairs
      :~  [%coll s+col.rum]
          [%who (ship who.rum)]
          [%data (total-build-to-json dat.rum)]
      ==
    ::
        %remove
      =/  suf=(list [@tas json])
      %-  pairs
      :~  [%coll s+col.rum]
          [%post ?~(pos.rum ~ s+u.pos.rum)]
      ==
    ==
    ::
  --
--
