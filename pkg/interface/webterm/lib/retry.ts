/**
 * Wait for the given milliseconds
 * @param {number} milliseconds The given time to wait
 * @returns {Promise} A fulfiled promise after the given time has passed
 */
 function waitFor(milliseconds) {
  return new Promise(resolve => setTimeout(resolve, milliseconds));
}

/**
 * Execute a promise and retry with exponential backoff
 * based on the maximum retry attempts it can perform
 * @param {Promise} promise promise to be executed
 * @param {function} onRetry callback executed on every retry
 * @param {number} maxRetries The maximum number of retries to be attempted
 * @returns {Promise} The result of the given promise passed in
 */
export function retry(promise, onRetry, maxRetries) {
  async function retryWithBackoff(retries) {
    try {
      if (retries > 0) {
        const timeToWait = 2 ** retries * 100;
        console.log(`waiting for ${timeToWait}ms...`);
        await waitFor(timeToWait);
      }
      return await promise();
    } catch (e) {
      if (retries < maxRetries) {
        onRetry();
        return retryWithBackoff(retries + 1);
      } else {
        console.warn('Max retries reached. Bubbling the error up');
        throw e;
      }
    }
  }

  return retryWithBackoff(0);
}
