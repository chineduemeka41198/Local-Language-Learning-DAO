import { assertEquals, types } from '@stacks/transactions';
import { Client, Provider, ProviderRegistry } from '@blockstack/clarity';
import { describe, it, before } from 'mocha';

describe('Local Language Learning DAO Test Suite', () => {
  let client: Client;
  let provider: Provider;

  before(async () => {
    provider = await ProviderRegistry.createProvider();
    client = new Client('Local-Language-Learning-DAO.clar', provider);
  });

  describe('Deployment', () => {
    it('should deploy successfully', async () => {
      await client.deployContract();
    });
  });

  describe('Content Submission', () => {
    it('should allow content submission with sufficient funds', async () => {
      const result = await client.submitContent('EN', 'hash123', 500);
      assertEquals(result.success, true);
    });

    it('should reject submission without sufficient funds', async () => {
      const result = await client.submitContent('ES', 'hash456', 0);
      assertEquals(result.error, 'ERR-INSUFFICIENT-FUNDS');
    });
  });

  describe('Content Verification', () => {
    it('should allow valid verifications', async () => {
      const result = await client.verifyContent(1);
      assertEquals(result.success, true);
    });

    it('should prevent self-verification', async () => {
      const result = await client.verifyContent(1);
      assertEquals(result.error, 'ERR-NOT-AUTHORIZED');
    });
  });
});