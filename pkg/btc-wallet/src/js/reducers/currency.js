import _ from 'lodash';

export class CurrencyReducer {
  reduce(json, state) {
    if (json.currencyRates) {
      for (var c in json.currencyRates) {
        state.currencyRates[c] = json.currencyRates[c];
      }
    }

    if (json.denomination) {
      if (state.currencyRates[json.denomination]) {
        state.denomination = json.denomination
      }
    }
  }
}
