export class UpdateReducer {
  reduce(json, state){
    console.log("update-reducer", json, state); 
    if (json.collection){
      if (json.collection.who === window.ship) {
        state.pubs[json.collection.coll] = {
          info: json.collection.data,
          order: { pin: [], unpin: [] },
          posts: {},
        }
      } else {

      }
    }
    
  }
}
