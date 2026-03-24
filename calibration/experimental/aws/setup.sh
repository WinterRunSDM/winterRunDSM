#!/bin/bash
# Sets up AWS infrastructure for remote calibration runs.
# Run once. Requires AWS CLI configured with admin-level credentials.
#
# Usage: bash calibration/aws/setup.sh --profile <profile> [--region <region>]

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
  echo "Usage: bash calibration/aws/setup.sh --profile <profile> [--region <region>]"
  exit 1
fi

AWS="aws --profile $PROFILE --region $REGION"

PROJECT="winter-run-dsm"
BUCKET="${PROJECT}-calibration"
ECR_REPO="${PROJECT}-calibration"
IAM_ROLE="${PROJECT}-calibration-ec2"
IAM_PROFILE="${PROJECT}-calibration-ec2"
SG_NAME="${PROJECT}-calibration"

ACCOUNT_ID=$($AWS sts get-caller-identity --query Account --output text)

echo "=== Winter Run DSM - AWS Setup ==="
echo "Profile:    $PROFILE"
echo "Region:     $REGION"
echo "Account:    $ACCOUNT_ID"
echo "=================================="

# 1. S3 Bucket
echo ""
echo "--- S3 Bucket ---"
if $AWS s3api head-bucket --bucket "$BUCKET" 2>/dev/null; then
  echo "Bucket $BUCKET already exists"
else
  $AWS s3api create-bucket \
    --bucket "$BUCKET" \
    --create-bucket-configuration LocationConstraint="$REGION"
  echo "Created bucket: $BUCKET"
fi

# 2. ECR Repository
echo ""
echo "--- ECR Repository ---"
if $AWS ecr describe-repositories --repository-names "$ECR_REPO" 2>/dev/null; then
  echo "ECR repo $ECR_REPO already exists"
else
  $AWS ecr create-repository \
    --repository-name "$ECR_REPO" \
    --image-scanning-configuration scanOnPush=false
  echo "Created ECR repo: $ECR_REPO"
fi

ECR_URI="${ACCOUNT_ID}.dkr.ecr.${REGION}.amazonaws.com/${ECR_REPO}"
echo "ECR URI: $ECR_URI"

# 3. IAM Role + Instance Profile
echo ""
echo "--- IAM Role ---"
AWS_IAM="aws --profile $PROFILE"

TRUST_POLICY='{
  "Version": "2012-10-17",
  "Statement": [{
    "Effect": "Allow",
    "Principal": {"Service": "ec2.amazonaws.com"},
    "Action": "sts:AssumeRole"
  }]
}'

if $AWS_IAM iam get-role --role-name "$IAM_ROLE" 2>/dev/null; then
  echo "IAM role $IAM_ROLE already exists"
else
  $AWS_IAM iam create-role \
    --role-name "$IAM_ROLE" \
    --assume-role-policy-document "$TRUST_POLICY"
  echo "Created IAM role: $IAM_ROLE"
fi

# Attach policies: S3 access + ECR pull
CALIBRATION_POLICY='{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "s3:GetObject",
        "s3:PutObject",
        "s3:ListBucket"
      ],
      "Resource": [
        "arn:aws:s3:::'"$BUCKET"'",
        "arn:aws:s3:::'"$BUCKET"'/*"
      ]
    },
    {
      "Effect": "Allow",
      "Action": [
        "ecr:GetAuthorizationToken",
        "ecr:BatchGetImage",
        "ecr:GetDownloadUrlForLayer",
        "ecr:BatchCheckLayerAvailability"
      ],
      "Resource": "*"
    }
  ]
}'

$AWS_IAM iam put-role-policy \
  --role-name "$IAM_ROLE" \
  --policy-name "${PROJECT}-calibration-policy" \
  --policy-document "$CALIBRATION_POLICY"
echo "Attached inline policy to role"

# Instance profile
if $AWS_IAM iam get-instance-profile --instance-profile-name "$IAM_PROFILE" 2>/dev/null; then
  echo "Instance profile $IAM_PROFILE already exists"
else
  $AWS_IAM iam create-instance-profile --instance-profile-name "$IAM_PROFILE"
  $AWS_IAM iam add-role-to-instance-profile \
    --instance-profile-name "$IAM_PROFILE" \
    --role-name "$IAM_ROLE"
  echo "Created instance profile: $IAM_PROFILE"
  echo "Waiting for instance profile to propagate..."
  sleep 10
fi

# 4. Security Group (default VPC, outbound only)
echo ""
echo "--- Security Group ---"
DEFAULT_VPC=$($AWS ec2 describe-vpcs \
  --filters Name=isDefault,Values=true \
  --query 'Vpcs[0].VpcId' --output text)

SG_ID=$($AWS ec2 describe-security-groups \
  --filters Name=group-name,Values="$SG_NAME" Name=vpc-id,Values="$DEFAULT_VPC" \
  --query 'SecurityGroups[0].GroupId' --output text 2>/dev/null || echo "None")

if [ "$SG_ID" != "None" ] && [ -n "$SG_ID" ]; then
  echo "Security group $SG_NAME already exists: $SG_ID"
else
  SG_ID=$($AWS ec2 create-security-group \
    --group-name "$SG_NAME" \
    --description "Calibration EC2 instances - outbound only" \
    --vpc-id "$DEFAULT_VPC" \
    --query 'GroupId' --output text)
  echo "Created security group: $SG_ID"
fi

# 5. Find latest Amazon Linux 2 AMI with Docker support
echo ""
echo "--- AMI ---"
AMI_ID=$($AWS ec2 describe-images \
  --owners amazon \
  --filters \
    "Name=name,Values=amzn2-ami-ecs-hvm-*-x86_64-ebs" \
    "Name=state,Values=available" \
  --query 'Images | sort_by(@, &CreationDate) | [-1].ImageId' \
  --output text)
echo "ECS-optimized AMI (has Docker): $AMI_ID"

# Summary
echo ""
echo "=== Setup Complete ==="
echo ""
echo "Add these to your .Renviron or set before running the Shiny app:"
echo ""
echo "CALIBRATION_S3_BUCKET=$BUCKET"
echo "CALIBRATION_ECR_IMAGE=${ECR_URI}:latest"
echo "CALIBRATION_AMI=$AMI_ID"
echo "CALIBRATION_SG=$SG_ID"
echo "CALIBRATION_IAM_PROFILE=$IAM_PROFILE"
echo "AWS_PROFILE=$PROFILE"
echo "AWS_REGION=$REGION"
echo ""
echo "Next step: push Docker image with:"
echo "  bash calibration/aws/push-image.sh --profile $PROFILE --region $REGION"
