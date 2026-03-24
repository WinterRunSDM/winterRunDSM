#!/bin/bash
# Build and push the calibration Docker image to ECR.
#
# Usage: bash calibration/aws/push-image.sh --profile <profile> [--region <region>]

set -euo pipefail

REGION="us-west-2"
PROFILE=""

while [[ $# -gt 0 ]]; do
  case $1 in
    --profile) PROFILE="$2"; shift 2 ;;
    --region) REGION="$2"; shift 2 ;;
    *) echo "Unknown option: $1"; exit 1 ;;
  esac
done

if [ -z "$PROFILE" ]; then
  echo "Usage: bash calibration/aws/push-image.sh --profile <profile> [--region <region>]"
  exit 1
fi

AWS="aws --profile $PROFILE --region $REGION"

ECR_REPO="winter-run-dsm-calibration"
ACCOUNT_ID=$($AWS sts get-caller-identity --query Account --output text)
ECR_URI="${ACCOUNT_ID}.dkr.ecr.${REGION}.amazonaws.com/${ECR_REPO}"

echo "Building Docker image..."
docker build -t "$ECR_REPO" .

echo "Logging in to ECR..."
$AWS ecr get-login-password | \
  docker login --username AWS --password-stdin "${ACCOUNT_ID}.dkr.ecr.${REGION}.amazonaws.com"

echo "Tagging and pushing..."
docker tag "${ECR_REPO}:latest" "${ECR_URI}:latest"
docker push "${ECR_URI}:latest"

echo "Pushed: ${ECR_URI}:latest"
